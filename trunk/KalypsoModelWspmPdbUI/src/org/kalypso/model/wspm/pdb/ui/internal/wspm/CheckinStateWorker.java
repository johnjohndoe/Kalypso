/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.pdb.ui.internal.wspm;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.content.ElementSelector;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.wspm.CheckinStateData;
import org.kalypso.model.wspm.pdb.wspm.CheckinStateOperation;
import org.kalypso.model.wspm.pdb.wspm.CheckinStatePrepareOperation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;

/**
 * @author Gernot Belger
 */
public class CheckinStateWorker implements ICheckInWorker
{
  private final CheckinStateData m_data;

  public CheckinStateWorker( final CommandableWorkspace workspace, final TuhhReach reach )
  {
    m_data = new CheckinStateData( workspace, reach );
  }

  @Override
  public IStatus checkPreconditions( )
  {
    final Map<String, BigDecimal> profileNames = new HashMap<>();

    final TuhhReach reach = m_data.getReach();

    /* Water Body must exist */
    if( m_data.getWaterBody() == null )
    {
      final WspmWaterBody wspmWaterBody = reach.getWaterBody();
      final String waterCode = wspmWaterBody.getRefNr();

      final String waterName = wspmWaterBody.getName();
      final String message = CheckInEventWorker.formatMissingWaterBody( waterCode, waterName );
      return new Status( IStatus.WARNING, WspmPdbUiPlugin.PLUGIN_ID, message );
    }

    /* Reach must not be empty */
    final TuhhReachProfileSegment[] reachProfileSegments = reach.getReachProfileSegments();
    if( reachProfileSegments.length == 0 )
    {
      final String message = Messages.getString( "CheckinStateWorker.2" ); //$NON-NLS-1$
      return new Status( IStatus.WARNING, WspmPdbUiPlugin.PLUGIN_ID, message );
    }

    /* Cross sections must consider unique constraints */
    for( final TuhhReachProfileSegment segment : reachProfileSegments )
    {
      final String segmentName = segment.getProfileMember().getName();
      final BigDecimal station = segment.getStation();

      final String name = CheckinStateOperation.createCrossSectionName( segmentName, station );

      if( StringUtils.isEmpty( name ) )
      {
        final String message = String.format( Messages.getString( "CheckinStateWorker.0" ), station ); //$NON-NLS-1$
        return new Status( IStatus.WARNING, WspmPdbUiPlugin.PLUGIN_ID, message );
      }

      if( profileNames.containsKey( name ) )
      {
        final BigDecimal otherStation = profileNames.get( name );
        final String message = String.format( Messages.getString( "CheckinStateWorker.1" ), station, otherStation, name ); //$NON-NLS-1$
        return new Status( IStatus.WARNING, WspmPdbUiPlugin.PLUGIN_ID, message );
      }

      profileNames.put( name, station );
    }

    return Status.OK_STATUS;
  }

  @Override
  public void preInit( final IPdbConnection connection ) throws PdbConnectException, CoreException
  {
    m_data.init( connection );
  }

  @Override
  public Wizard createWizard( )
  {
    return new CheckinStateWizard( this, m_data );
  }

  @Override
  public void configureSelector( final ElementSelector selector )
  {
    final String newStateName = m_data.getState().getName();
    selector.addStateName( newStateName );
  }

  @Override
  public void closeConnection( )
  {
    m_data.closeConnection();
  }

  @Override
  public boolean performFinish( final IWizardContainer container )
  {
    final CheckinStatePrepareOperation prepareOperation = new CheckinStatePrepareOperation( m_data );
    try
    {
      final Shell shell = container.getShell();
      final String windowTitle = shell.getText();

      /* Prepare: open session and check if status is new or will be updated */
      final IStatus prepareStatus = RunnableContextHelper.execute( container, true, true, prepareOperation );
      if( !prepareStatus.isOK() )
      {
        StatusDialog.open( shell, prepareStatus, windowTitle );
        return false;
      }

      /* If update: warn user and ask to proceed */
      if( !prepareOperation.warnUser( shell, windowTitle ) )
        return false;

      final CheckinStateOperation checkinOperation = prepareOperation.createCheckinOperation();

      final IStatus checkinStatus = RunnableContextHelper.execute( container, true, true, checkinOperation );
      if( !checkinStatus.isOK() )
        StatusDialog.open( shell, checkinStatus, windowTitle );

      return !checkinStatus.matches( IStatus.ERROR );
    }
    finally
    {
      prepareOperation.dispose();
    }
  }
}