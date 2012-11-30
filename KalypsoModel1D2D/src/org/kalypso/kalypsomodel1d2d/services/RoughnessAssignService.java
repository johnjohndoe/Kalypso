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

import org.apache.commons.lang3.ObjectUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.model.ICommandPoster;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygon;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygonCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Position;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Dejan Antanaskovic
 */
public class RoughnessAssignService extends Job
{
  private final List<IRoughnessPolygonCollection> m_roughnessPolygonCollections;

  private final IFEDiscretisationModel1d2d m_model1d2d;

  private final GM_Envelope m_workArea;

  public RoughnessAssignService( final String name, final ITerrainModel terrainModel, final IFEDiscretisationModel1d2d model1d2d, final GM_Envelope workArea )
  {
    super( name );

    m_model1d2d = model1d2d;
    m_workArea = workArea;
    m_roughnessPolygonCollections = terrainModel.getRoughnessPolygonCollections();
  }

  @Override
  protected IStatus run( final IProgressMonitor monitor )
  {
    final List<FeatureChange> allChanges = new ArrayList<>();

    try
    {
      final IFE1D2DElement[] elementsInWorkarea;
      if( m_workArea != null )
      {
        final List<IFE1D2DElement> els = m_model1d2d.queryElements( m_workArea, null );
        elementsInWorkarea = els.toArray( new IFE1D2DElement[els.size()] );
      }
      else
        elementsInWorkarea = m_model1d2d.getElements();
      final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString( "org.kalypso.kalypsomodel1d2d.services.RoughnessAssignService.0" ), elementsInWorkarea.length ); //$NON-NLS-1$

      for( final IFE1D2DElement element : elementsInWorkarea )
      {
        if( (element instanceof IPolyElement) )
          assignRoughness( allChanges, (IPolyElement)element );

        // if( monitor.isCanceled() )
        // return Status.CANCEL_STATUS;
        // TODO: do not cancel, does not work well with work area...
        // TODO: change this stuff, so that we have a queue of elements to be worked on
        // ProgressUtilities.worked( progress, 1 );
        progress.worked( 1 );
      }
    }
    catch( final Throwable t )
    {
      // return StatusUtilities.statusFromThrowable( t, "Roughness asigning not finished." );
      t.printStackTrace();
    }
    finally
    {
      monitor.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.services.RoughnessAssignService.1" ) ); //$NON-NLS-1$
      postCommands( allChanges );
      monitor.done();
    }
    return Status.OK_STATUS;
  }

  private void assignRoughness( final List<FeatureChange> allChanges, final IPolyElement element )
  {
    final GM_Polygon geometryProperty = element.getGeometry();

    if( geometryProperty == null )
      return;

    final GM_Point centroid = geometryProperty.getCentroid();
    final GM_Position position = centroid.getPosition();

    String roughnessClsID = null;
    String roughnessStyle = IRoughnessPolygon.NO_ROUGHNESS;
    Double correctionParameterKS = null;
    Double correctionParameterAxAy = null;
    Double correctionParameterDP = null;

    for( int i = 0; i < m_roughnessPolygonCollections.size(); i++ )
    {
      // if( monitor.isCanceled() )
      // throw new CoreException( Status.CANCEL_STATUS );

      final IRoughnessPolygonCollection roughnessPolygonCollection = m_roughnessPolygonCollections.get( i );
      final List<IRoughnessPolygon> matchedRoughness = roughnessPolygonCollection.query( position );
      if( matchedRoughness == null || matchedRoughness.size() == 0 )
        continue;

      // (explanation: for the loop) because if we have overlapped polygons in the same layer,
      // we want to assign roughness exacly like (overlapped) polygons are drawn on the map

      // later: get rid of overlapping!!! :)

      for( int j = matchedRoughness.size() - 1; j >= 0; j-- )
      {
        final IRoughnessPolygon roughnessPolygon = matchedRoughness.get( j );
        if( roughnessPolygon.getSurface().contains( position ) )
        {
          if( correctionParameterKS == null )
          {
            final Double check = roughnessPolygon.getCorrectionParameterKS();
            if( check != null && !check.isNaN() )
            {
              correctionParameterKS = check;
            }
          }

          if( correctionParameterAxAy == null )
          {
            final Double check = roughnessPolygon.getCorrectionParameterAxAy();
            if( check != null && !check.isNaN() )
              correctionParameterAxAy = check;
          }

          if( correctionParameterDP == null )
          {
            final Double check = roughnessPolygon.getCorrectionParameterDP();
            if( check != null && !check.isNaN() )
              correctionParameterDP = check;
          }

          if( roughnessClsID == null )
          {
            final IRoughnessCls roughnessCls = roughnessPolygon.getRoughnessCls();
            if( roughnessCls != null )
            {
              roughnessClsID = roughnessCls.getId();
              roughnessStyle = roughnessPolygon.getRoughnessStyle();
            }
          }
        }
      }
    }

    assignRoughness( allChanges, element, roughnessClsID, correctionParameterKS, correctionParameterAxAy, correctionParameterDP, roughnessStyle );
  }

  private void assignRoughness( final List<FeatureChange> allChanges, final IPolyElement element, final String roughnessClsID, final Double correctionParameterKS, final Double correctionParameterAxAy, final Double correctionParameterDP, final String roughnessStyle )
  {
    final String elementRoughnessClsID = element.getRoughnessClsID();
    if( !ObjectUtils.equals( elementRoughnessClsID, roughnessClsID ) )
      allChanges.add( new FeatureChange( element, IPolyElement.PROP_ROUGHNESS_CLS_ID, roughnessClsID ) );

    final String elementRoughnessStyle = element.getRoughnessStyle();
    if( !ObjectUtils.equals( elementRoughnessStyle, roughnessStyle ) )
      allChanges.add( new FeatureChange( element, IPolyElement.PROP_ROUGHNESS_STYLE, roughnessStyle ) );

    final Double elementRoughnessCorrectionKS = element.getRoughnessCorrectionKS();
    if( !ObjectUtils.equals( elementRoughnessCorrectionKS, correctionParameterKS ) )
      allChanges.add( new FeatureChange( element, IPolyElement.PROP_ROUGHNESS_CORRECTION_KS, correctionParameterKS ) );

    final Double elementRoughnessCorrectionAxAy = element.getRoughnessCorrectionAxAy();
    if( !ObjectUtils.equals( elementRoughnessCorrectionAxAy, correctionParameterAxAy ) )
      allChanges.add( new FeatureChange( element, IPolyElement.PROP_ROUGHNESS_CORRECTION_AXAY, correctionParameterAxAy ) );

    final Double elementRoughnessCorrectionDP = element.getRoughnessCorrectionDP();
    if( !ObjectUtils.equals( elementRoughnessCorrectionDP, correctionParameterDP ) )
      allChanges.add( new FeatureChange( element, IPolyElement.PROP_ROUGHNESS_CORRECTION_DP, correctionParameterDP ) );
  }

  private void postCommands( final List<FeatureChange> changes )
  {
    if( changes.size() == 0 )
      return;

    try
    {
      final GMLWorkspace workspace = m_model1d2d.getWorkspace();
      final IScenarioDataProvider modelProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();

      final RoughnessAssignCommand command = new RoughnessAssignCommand( workspace, changes.toArray( new FeatureChange[changes.size()] ) );

      ((ICommandPoster)modelProvider).postCommand( IFEDiscretisationModel1d2d.class.getName(), command );
    }
    catch( final Exception e1 )
    {
      e1.printStackTrace();
    }
  }
}