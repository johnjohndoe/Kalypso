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
package org.kalypso.kalypsomodel1d2d.services;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygonCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.command.FeatureChangeModellEvent;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;

/**
 * @author antanas
 * 
 */
public class RoughnessAssignService extends Job
{
  private static final String DEFAULT_ROUGHNESS_STYLE = "_DEFAULT_STYLE_";

  private List<IRoughnessPolygonCollection> m_roughnessPolygonCollections;

  private final IFEDiscretisationModel1d2d m_model1d2d;

  private List<FeatureChange> m_changes = new ArrayList<FeatureChange>();

  public RoughnessAssignService( final String name, final ITerrainModel terrainModel, final IFEDiscretisationModel1d2d model1d2d )
  {
    super( name );
    m_model1d2d = model1d2d;
    m_roughnessPolygonCollections = terrainModel.getRoughnessPolygonCollections();
  }

  /**
   * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected IStatus run( IProgressMonitor monitor )
  {
    try
    {
      final SubMonitor progress = SubMonitor.convert( monitor, "Roughness asigning", 100 );

      ProgressUtilities.worked( progress, 0 );

      for( final IFE1D2DElement element : m_model1d2d.getElements() )
      {
        final boolean roughnessFound = assignRoughness( monitor, element );
        if( !roughnessFound )
        {
          final FeatureChange[] changes = element.assignRoughness( "", Double.NaN, Double.NaN, Double.NaN, DEFAULT_ROUGHNESS_STYLE );
          for( FeatureChange featureChange : changes )
            m_changes.add( featureChange );
        }
      }
    }
    catch( final Throwable t )
    {
      return StatusUtilities.statusFromThrowable( t, "Roughness asigning not finished." );
    }
    finally
    {
      fireEvents();

      monitor.done();
    }
    return Status.OK_STATUS;
  }

  private boolean assignRoughness( IProgressMonitor monitor, final IFE1D2DElement element ) throws CoreException
  {
    for( int i = m_roughnessPolygonCollections.size() - 1; i >= 0; i-- )
    {
      if( monitor.isCanceled() )
        throw new CoreException( Status.CANCEL_STATUS );

      final IRoughnessPolygonCollection roughnessPolygonCollection = m_roughnessPolygonCollections.get( i );
      final GM_Point centroid = element.getWrappedFeature().getDefaultGeometryProperty().getCentroid();
      final GM_Position position = centroid.getPosition();
      final List<IRoughnessPolygon> matchedRoughness = roughnessPolygonCollection.query( position );

      if( matchedRoughness == null || matchedRoughness.size() == 0 )
        continue;

      for( IRoughnessPolygon roughnessPolygon : matchedRoughness )
      {
        if( roughnessPolygon.getSurface().contains( position ) )
        {
          final String roughnessClsGmlID = roughnessPolygon.getRoughnessCls().getGmlID();
          final Double correctionParameterKS = roughnessPolygon.getCorrectionParameterKS();
          final Double correctionParameterAxAy = roughnessPolygon.getCorrectionParameterAxAy();
          final Double correctionParameterDP = roughnessPolygon.getCorrectionParameterDP();
          final String roughnessStyle = roughnessPolygon.getRoughnessStyle();
          
          final FeatureChange[] changes = element.assignRoughness( roughnessClsGmlID, correctionParameterKS, correctionParameterAxAy, correctionParameterDP, roughnessStyle );
          for( FeatureChange featureChange : changes )
            m_changes.add( featureChange );
          return true;
        }
      }
    }
    return false;
  }

  private void fireEvents( )
  {
    final GMLWorkspace workspace = m_model1d2d.getWrappedFeature().getWorkspace();

    final FeatureChange[] changes = m_changes.toArray( new FeatureChange[m_changes.size()] );
    final FeatureChangeModellEvent event = new RoughnessAssignServiceModellEvent( workspace, changes );

    workspace.fireModellEvent( event );
  }
}
