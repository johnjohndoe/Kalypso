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

import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.wspm.core.gml.WspmFixation;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.content.ElementSelector;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.wspm.CheckInEventData;
import org.kalypso.model.wspm.pdb.wspm.CheckInEventOperation;
import org.kalypso.model.wspm.pdb.wspm.SaveEventOperation;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;

/**
 * @author Gernot Belger
 */
public class CheckInEventWorker implements ICheckInWorker
{
  private final CheckInEventData<WspmFixation> m_data;

  public CheckInEventWorker( final CommandableWorkspace workspace, final WspmFixation fixation )
  {
    m_data = new CheckInEventData<WspmFixation>( workspace, fixation )
    {
      @Override
      public WaterBody findWaterBody( )
      {
        final Map<String, WaterBody> waterHash = getWaterHash();

        final WspmWaterBody wspmWaterBody = fixation.getOwner();
        final String waterCode = wspmWaterBody.getRefNr();

        return waterHash.get( waterCode );
      }
    };
  }

  @Override
  public IStatus checkPreconditions( )
  {
    /* Water Body must exist */
    final WaterBody waterBody = m_data.findWaterBody();
    if( waterBody == null )
    {
      final WspmFixation fixation = m_data.getWspmObject();

      final WspmWaterBody wspmWaterBody = fixation.getOwner();
      final String waterCode = wspmWaterBody.getRefNr();

      final String waterName = wspmWaterBody.getName();
      final String message = formatMissingWaterBody( waterCode, waterName );
      return new Status( IStatus.WARNING, WspmPdbUiPlugin.PLUGIN_ID, message );
    }

    return Status.OK_STATUS;
  }

  static String formatMissingWaterBody( final String waterCode, final String waterName )
  {
    return String.format( Messages.getString( "CheckInEventWorker.0" ), waterName, waterCode ); //$NON-NLS-1$
  }

  @Override
  public void preInit( final IPdbConnection connection ) throws PdbConnectException
  {
    m_data.init( connection );
  }

  @Override
  public Wizard createWizard( )
  {
    return new CheckInEventWizard( this, m_data );
  }

  @Override
  public void configureSelector( final ElementSelector selector )
  {
    selector.addEventId( m_data.getEvent().getId() );
  }

  @Override
  public void closeConnection( )
  {
    m_data.closeConnection();
  }

  @Override
  public boolean performFinish( final IWizardContainer container )
  {
    if( !SaveEventOperation.askForEmptyState( m_data.getEvent(), container.getShell(), container.getCurrentPage().getWizard().getWindowTitle() ) )
      return false;

    final CheckInEventOperation operation = new CheckInEventOperation( m_data );

    // FIXME: if wizard is not closed due to error, we need to reinitialize the event, as it is still attached to the old session

    return executeOnContainer( container, operation );
  }

  // TODO: move into helper
  static boolean executeOnContainer( final IWizardContainer container, final ICoreRunnableWithProgress operation )
  {
    final IStatus status = RunnableContextHelper.execute( container, true, true, operation );
    if( !status.isOK() )
    {
      final Shell shell = container.getShell();
      final String windowTitle = shell.getText();

      StatusDialog.open( shell, status, windowTitle );
    }

    return !status.matches( IStatus.ERROR );
  }
}