/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.pdb.wspm;

import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashSet;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.pdb.connect.Executor;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.gaf.Coefficients;
import org.kalypso.model.wspm.pdb.internal.gaf.GafCodes;
import org.kalypso.model.wspm.pdb.internal.wspm.CheckinStatePdbOperation;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class CheckinStateOperation implements ICoreRunnableWithProgress
{
  private static final String STR_FAILED_TO_WRITE_TO_DATABASE = "Failed to write to database";

  private final CheckinStateData m_data;

  private final IPdbConnection m_connection;

  public CheckinStateOperation( final CheckinStateData data, final IPdbConnection connection )
  {
    m_data = data;
    m_connection = connection;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "Upload cross sections into database", 100 );

    final IProfileFeature[] profiles = findProfiles();

    Session session = null;

    try
    {
      session = m_connection.openSession();

      final GafCodes gafCodes = new GafCodes();
      final WaterBody[] waterBodies = m_data.getExistingWaterBodies();
      final State state = m_data.getState();
      final String dbSrs = m_data.getDatabaseSrs();
      final Coefficients coefficients = m_data.getCoefficients();
      state.setEditingUser( m_connection.getSettings().getUsername() );
      final URI documentBase = m_data.getDocumentBase();
      m_connection.getInfo().getDocumentServer();

      final CheckinStatePdbOperation operation = new CheckinStatePdbOperation( gafCodes, coefficients, waterBodies, state, profiles, dbSrs, documentBase, new SubProgressMonitor( monitor, 90 ) );
      new Executor( session, operation ).execute();

      session.close();

      updateReach( state );
    }
    catch( final HibernateException e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, STR_FAILED_TO_WRITE_TO_DATABASE, e );
      throw new CoreException( status );
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, STR_FAILED_TO_WRITE_TO_DATABASE, e );
      throw new CoreException( status );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, "Failed to initialize GAF codes", e );
      throw new CoreException( status );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, "Operation failed", e );
      throw new CoreException( status );
    }
    finally
    {
      monitor.done();
    }

    return Status.OK_STATUS;
  }

  /**
   * Some properties of the state may have changed in the wizard. We update the local reach with these properties.
   */
  private void updateReach( final State state ) throws Exception
  {
    final String name = state.getName();
    final String description = state.getDescription();

    final TuhhReach reach = m_data.getReach();

    final FeatureChange nameChange = new FeatureChange( reach, Feature.QN_NAME, new ArrayList<String>( Collections.singletonList( name ) ) );
    final FeatureChange descChange = new FeatureChange( reach, Feature.QN_DESCRIPTION, description );

    final CommandableWorkspace workspace = m_data.getWorkspace();
    final ChangeFeaturesCommand command = new ChangeFeaturesCommand( workspace, new FeatureChange[] { nameChange, descChange } );
    workspace.postCommand( command );
  }

  private IProfileFeature[] findProfiles( )
  {
    final TuhhReach reach = m_data.getReach();

    final LinkedHashSet<IProfileFeature> profiles = new LinkedHashSet<IProfileFeature>();
    final TuhhReachProfileSegment[] segments = reach.getReachProfileSegments();
    for( final TuhhReachProfileSegment segment : segments )
    {
      final IProfileFeature profile = segment.getProfileMember();
      profiles.add( profile );
    }

    return profiles.toArray( new IProfileFeature[profiles.size()] );
  }
}