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
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.content.ElementSelector;
import org.kalypso.model.wspm.pdb.wspm.CheckinStateData;
import org.kalypso.model.wspm.pdb.wspm.CheckinStateOperation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;

/**
 * @author Gernot Belger
 */
public class CheckinStateWorker implements ICheckInWorker
{
  private final CheckinStateData m_data;

  private final CheckinStateOperation m_operation;

  public CheckinStateWorker( final CommandableWorkspace workspace, final TuhhReach reach )
  {
    m_data = new CheckinStateData( workspace, reach );
    m_operation = new CheckinStateOperation( m_data );
  }

  @Override
  public IStatus checkPreconditions( )
  {
    final Map<String, BigDecimal> profileNames = new HashMap<String, BigDecimal>();

    final Set<String> existingWaterCodes = hashWaterCodes( m_data.getExistingWaterBodies() );

    final TuhhReach reach = m_data.getReach();

    final WspmWaterBody wspmWaterBody = reach.getWaterBody();
    final String waterCode = wspmWaterBody.getRefNr();
    /* Water Body must exist */
    if( !existingWaterCodes.contains( waterCode ) )
    {
      final String waterName = wspmWaterBody.getName();
      final String message = CheckInEventWorker.formatMissingWaterBody( waterCode, waterName );
      return new Status( IStatus.WARNING, WspmPdbUiPlugin.PLUGIN_ID, message );
    }

    final TuhhReachProfileSegment[] reachProfileSegments = reach.getReachProfileSegments();
    for( final TuhhReachProfileSegment segment : reachProfileSegments )
    {
      final String name = segment.getProfileMember().getName();
      final BigDecimal station = segment.getStation();

      if( StringUtils.isEmpty( name ) )
      {
        final String message = String.format( "Cross section at km %s has no name.%nOnly cross sections with a name can be uploaded into the database.", station );
        return new Status( IStatus.WARNING, WspmPdbUiPlugin.PLUGIN_ID, message );
      }

      if( profileNames.containsKey( name ) )
      {
        final BigDecimal otherStation = profileNames.get( name );
        final String message = String.format( "Cross sections at km %s and %s have the same name: '%s'%nNames must be unique in the database.", station, otherStation, name );
        return new Status( IStatus.WARNING, WspmPdbUiPlugin.PLUGIN_ID, message );
      }

      profileNames.put( name, station );
    }

    return Status.OK_STATUS;
  }

  static Set<String> hashWaterCodes( final WaterBody[] waterBodies )
  {
    final Set<String> codes = new HashSet<String>();

    for( final WaterBody waterBody : waterBodies )
      codes.add( waterBody.getName() );

    return Collections.unmodifiableSet( codes );
  }

  @Override
  public void preInit( final IPdbConnection connection ) throws PdbConnectException, CoreException
  {
    m_data.init( connection );
  }

  @Override
  public Wizard createWizard( )
  {
    return new CheckinStateWizard( m_data, m_operation );
  }

  @Override
  public void configureSelector( final ElementSelector selector )
  {
    final String newStateName = m_data.getState().getName();
    selector.addStateName( newStateName );
  }

  @Override
  public ICoreRunnableWithProgress getOperation( )
  {
    return m_operation;
  }

  @Override
  public void closeConnection( )
  {
    m_data.closeConnection();
  }
}
