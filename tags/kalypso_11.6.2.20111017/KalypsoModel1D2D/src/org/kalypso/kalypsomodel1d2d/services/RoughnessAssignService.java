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

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
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
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;

import de.renew.workflow.connector.cases.CaseHandlingSourceProvider;
import de.renew.workflow.connector.cases.ICaseDataProvider;

/**
 * @author Dejan Antanaskovic
 */
public class RoughnessAssignService extends Job
{
  private final List<IRoughnessPolygonCollection> m_roughnessPolygonCollections;

  private final IFEDiscretisationModel1d2d m_model1d2d;

  private final List<FeatureChange> m_changesDiscretisationModel = new ArrayList<FeatureChange>();

  private GM_Envelope m_workArea;

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
  protected IStatus run( final IProgressMonitor monitor )
  {
    try
    {
      final List<IFE1D2DElement> elementsInWorkarea = (m_workArea != null) ? m_model1d2d.getElements().query( m_workArea ) : m_model1d2d.getElements();
      final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString("org.kalypso.kalypsomodel1d2d.services.RoughnessAssignService.0"), elementsInWorkarea.size() ); //$NON-NLS-1$
// ProgressUtilities.worked( progress, 0 );
      m_changesDiscretisationModel.clear();
      for( final IFE1D2DElement element : elementsInWorkarea )
      {
        assignRoughness( monitor, element );
//        if( monitor.isCanceled() )
//          return Status.CANCEL_STATUS;
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
      monitor.subTask( Messages.getString("org.kalypso.kalypsomodel1d2d.services.RoughnessAssignService.1") ); //$NON-NLS-1$
      fireEvents();
      monitor.done();
    }
    return Status.OK_STATUS;
  }

  private void assignRoughness( final IProgressMonitor monitor, final IFE1D2DElement element ) throws CoreException
  {
    if( !(element instanceof IPolyElement) )
      return;

    final GM_Object geometryProperty = ((IPolyElement) element).getGeometry();

    // this might happen if 2d network is just imported, and new elements are created
    // (by the widget) before saving anything
    if( geometryProperty == null )
      return;

    final GM_Point centroid = geometryProperty.getCentroid();
    final GM_Position position = centroid.getPosition();
    boolean missingRoughnessClsID = true;
    boolean missingRoughnessCorrectionKS = true;
    boolean missingRoughnessCorrectionAxAy = true;
    boolean missingRoughnessCorrectionDP = true;
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

      // for( final IRoughnessPolygon roughnessPolygon : matchedRoughness )
      for( int j = matchedRoughness.size() - 1; j >= 0; j-- )
      {
        final IRoughnessPolygon roughnessPolygon = matchedRoughness.get( j );
        if( roughnessPolygon.getSurface().contains( position ) )
        {
          if( missingRoughnessCorrectionKS )
          {
            final Double check = roughnessPolygon.getCorrectionParameterKS();
            if( check != null && !check.isNaN() )
            {
              correctionParameterKS = check;
              missingRoughnessCorrectionKS = false;
            }
          }
          if( missingRoughnessCorrectionAxAy )
          {
            final Double check = roughnessPolygon.getCorrectionParameterAxAy();
            if( check != null && !check.isNaN() )
            {
              correctionParameterAxAy = check;
              missingRoughnessCorrectionAxAy = false;
            }
          }
          if( missingRoughnessCorrectionDP )
          {
            final Double check = roughnessPolygon.getCorrectionParameterDP();
            if( check != null && !check.isNaN() )
            {
              correctionParameterDP = check;
              missingRoughnessCorrectionDP = false;
            }
          }
          if( missingRoughnessClsID )
          {
            final IRoughnessCls roughnessCls = roughnessPolygon.getRoughnessCls();
            if( roughnessCls != null )
            {
              roughnessClsID = roughnessCls.getGmlID();
              roughnessStyle = roughnessPolygon.getRoughnessStyle();
              missingRoughnessClsID = false;
            }
          }
        }
      }
    }
    boolean anyChanges = false;
    final String elementRoughnessClsID = element.getRoughnessClsID();
    final String elementRoughnessStyle = element.getRoughnessStyle();
    final Double elementRoughnessCorrectionKS = element.getRoughnessCorrectionKS();
    final Double elementRoughnessCorrectionAxAy = element.getRoughnessCorrectionAxAy();
    final Double elementRoughnessCorrectionDP = element.getRoughnessCorrectionDP();
    anyChanges |= elementRoughnessClsID != null && elementRoughnessClsID.length() > 0 && !elementRoughnessClsID.equals( roughnessClsID );
    anyChanges |= !elementRoughnessStyle.equals( roughnessStyle );
    anyChanges |= elementRoughnessCorrectionKS != null && elementRoughnessCorrectionKS != correctionParameterKS;
    anyChanges |= elementRoughnessCorrectionAxAy != null && elementRoughnessCorrectionAxAy != correctionParameterAxAy;
    anyChanges |= elementRoughnessCorrectionDP != null && elementRoughnessCorrectionDP != correctionParameterDP;
    if( anyChanges )
    {
      final FeatureChange[] changes = element.assignRoughness( roughnessClsID, correctionParameterKS, correctionParameterAxAy, correctionParameterDP, roughnessStyle );
      for( final FeatureChange featureChange : changes )
        m_changesDiscretisationModel.add( featureChange );
    }
  }

  private void fireEvents( )
  {
    if( m_changesDiscretisationModel.size() > 0 )
    {
      final GMLWorkspace workspace = m_model1d2d.getFeature().getWorkspace();
      final IWorkbench workbench = PlatformUI.getWorkbench();
      final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
      final IEvaluationContext context = handlerService.getCurrentState();
      final ICaseDataProvider<IFeatureWrapper2> modelProvider = (ICaseDataProvider<IFeatureWrapper2>) context.getVariable( CaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
      final RoughnessAssignCommand command = new RoughnessAssignCommand( workspace, m_changesDiscretisationModel.toArray( new FeatureChange[m_changesDiscretisationModel.size()] ) );
      try
      {
        ((ICommandPoster) modelProvider).postCommand( IFEDiscretisationModel1d2d.class, command );
      }
      catch( final Exception e1 )
      {
        // TODO Auto-generated catch block
        e1.printStackTrace();
      }
    }
  }

  /**
   * Defines the area where roughness recalculation is needed; if <code>null</code>, whole model will be recalculated
   */
  public void setWorkarea( final GM_Envelope envelope )
  {
    m_workArea = envelope;
  }
}
