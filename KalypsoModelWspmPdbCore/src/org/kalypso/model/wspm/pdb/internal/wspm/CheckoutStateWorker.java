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
package org.kalypso.model.wspm.pdb.internal.wspm;

import java.net.URI;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmReach;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.wspm.CheckoutDataMapping;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Gernot Belger
 */
public class CheckoutStateWorker
{
  private final CheckoutDataMapping m_mapping;

  private final URI m_documentBase;

  public CheckoutStateWorker( final CheckoutDataMapping mapping, final URI documentBase )
  {
    m_mapping = mapping;
    m_documentBase = documentBase;
  }

  public void execute( final IProgressMonitor monitor ) throws CoreException
  {
    final State[] states = m_mapping.getStates();

    monitor.beginTask( Messages.getString( "CheckoutStateWorker.0" ), states.length ); //$NON-NLS-1$

    try
    {
      for( final State state : states )
      {
        final TuhhReach reach = m_mapping.getReach( state );
        final TuhhReach newReach = createOrReplaceReach( state, reach );

        m_mapping.set( state, newReach );
        m_mapping.addAddedFeatures( newReach );

        monitor.worked( 1 );
      }
    }
    catch( final GMLSchemaException e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, "Should never happen", e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
    finally
    {
      ProgressUtilities.done( monitor );
    }
  }

  private TuhhReach createOrReplaceReach( final State state, final TuhhReach reach ) throws GMLSchemaException
  {
    final WaterBody waterBody = m_mapping.findWaterBody( state );
    final WspmWaterBody wspmWater = m_mapping.getWspmWaterBody( waterBody );

    removeReach( reach, wspmWater );

    return insertReach( state, wspmWater );
  }

  private void removeReach( final TuhhReach reach, final WspmWaterBody wspmWater )
  {
    if( reach == null )
      return;

    final IFeatureBindingCollection<WspmReach> reaches = wspmWater.getReaches();
    reaches.remove( reach );

    cleanupReachProfiles( reach );

    m_mapping.addRemovedFeatures( reach );
  }

  /**
   * Cleanup profiles from this reach (which will be deleted), that are not referenced elsewhere.
   */
  private void cleanupReachProfiles( final TuhhReach reach )
  {
    final WspmWaterBody waterBody = reach.getWaterBody();
    if( waterBody == null )
      return;

    final ReachProfileReferencer referencer = new ReachProfileReferencer( waterBody );

    final TuhhReachProfileSegment[] reachProfileSegments = reach.getReachProfileSegments();
    for( final TuhhReachProfileSegment segment : reachProfileSegments )
    {
      final IProfileFeature profileMember = segment.getProfileMember();
      if( !referencer.isReferenced( profileMember ) )
      {
        waterBody.getProfiles().remove( profileMember );
        m_mapping.addRemovedFeatures( profileMember );
      }
    }
  }

  private TuhhReach insertReach( final State state, final WspmWaterBody waterBody ) throws GMLSchemaException
  {
    final String name = state.getName();

    final TuhhReach newReach = TuhhWspmProject.createNewReachForWaterBody( waterBody );
    newReach.setName( name );
    newReach.setDescription( state.getDescription() );

    final DocumentConverter documentConverter = new DocumentConverter( m_documentBase );
    documentConverter.convertDocuments( state, newReach );

    return newReach;
  }
}