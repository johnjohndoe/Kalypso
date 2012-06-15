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
package org.kalypso.kalypsomodel1d2d.ops;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.model.ICommandPoster;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.FlowRelationUtilitites;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

import de.renew.workflow.connector.cases.CaseHandlingSourceProvider;
import de.renew.workflow.connector.cases.ICaseDataProvider;

/**
 * 
 * @author Dejan Antanaskovic
 * 
 */

public class GeometryRecalculator
{
  private boolean m_nodesAdded = false;

  private final List<Feature> m_nodeList = new ArrayList<Feature>();

  private final List<Feature> m_discretisationModelChanges = new ArrayList<Feature>();

  private final List<Feature> m_flowRelationshipsModelChanges = new ArrayList<Feature>();

  private final IFlowRelationshipModel m_flowRelCollection;

  private final IKalypsoFeatureTheme m_flowRelationsModelTheme;

  public GeometryRecalculator( final Collection<Feature> changedFeatures, final IKalypsoFeatureTheme flowRelationsModelTheme )
  {
    m_flowRelationsModelTheme = flowRelationsModelTheme;
    final FeatureList featureList = m_flowRelationsModelTheme.getFeatureList();
    final Feature parentFeature = featureList.getParentFeature();
    m_flowRelCollection = (IFlowRelationshipModel) parentFeature.getAdapter( IFlowRelationshipModel.class );
    init( changedFeatures );
  }

  @SuppressWarnings("unchecked")
  private void init( final Collection<Feature> changedFeatures )
  {
    for( final Feature feature : changedFeatures )
    {
      if( TypeInfo.isNode( feature ) )
        addToNodes( (IFE1D2DNode) feature.getAdapter( IFE1D2DNode.class ) );
      else if( TypeInfo.isElement1DFeature( feature ) )
      {
        final IElement1D element1D = (IElement1D) feature.getAdapter( IElement1D.class );
        final List<IFE1D2DNode> nodes = element1D.getNodes();
        for( final IFE1D2DNode node : nodes )
          addToNodes( node );
      }
      else if( TypeInfo.isPolyElementFeature( feature ) )
      {
        final IPolyElement<IFE1D2DComplexElement, IFE1D2DEdge> polyElement = (IPolyElement) feature.getAdapter( IPolyElement.class );
        final List<IFE1D2DNode> nodes = polyElement.getNodes();
        for( final IFE1D2DNode node : nodes )
          addToNodes( node );
      }
    }
  }

  private void addToNodes( final IFE1D2DNode node )
  {
    final Feature feature = node.getFeature();
    if( m_nodeList.contains( feature ) )
      return;
    addToDiscretisationModelChanges( node );
    m_nodeList.add( feature );
    m_nodesAdded = true;
  }

  private void addToDiscretisationModelChanges( final IFeatureWrapper2 element )
  {
    final Feature feature = element.getFeature();
    if( m_discretisationModelChanges.contains( feature ) )
      return;
    feature.invalidEnvelope();
    m_discretisationModelChanges.add( feature );
    final String featureID = feature.getId();
    for( final Object object : m_flowRelCollection.getWrappedList() )
    {
      final IBoundaryCondition boundaryCondition = (IBoundaryCondition) ((Feature) object).getAdapter( IBoundaryCondition.class );
      if( boundaryCondition != null )
      {
        if( boundaryCondition.getParentElementID().equals( featureID ) )
          addToFlowRelationshipsModelChanges( boundaryCondition, element );
        continue;
      }

    }
  }

  private void addToFlowRelationshipsModelChanges( final IBoundaryCondition boundaryCondition, final IFeatureWrapper2 element )
  {
    final Feature feature = boundaryCondition.getFeature();
    if( m_flowRelationshipsModelChanges.contains( feature ) )
      return;
    feature.invalidEnvelope();
    final String crs = KalypsoCorePlugin.getDefault().getCoordinatesSystem();
    if( element instanceof IFELine )
    {
      int countBCs = 0;
      for( final Object bcFeature : m_flowRelCollection.getWrappedList() )
      {
        final IBoundaryCondition bc = (IBoundaryCondition) ((Feature) bcFeature).getAdapter( IBoundaryCondition.class );
        if( bc.getParentElementID().equals( element.getGmlID() ) )
          countBCs++;
      }
      int i = 0;
      for( final Object bcFeature : m_flowRelCollection.getWrappedList() )
      {
        final IBoundaryCondition bc = (IBoundaryCondition) ((Feature) bcFeature).getAdapter( IBoundaryCondition.class );
        if( bc.getParentElementID().equals( element.getGmlID() ) )
        {
          final GM_Position position = FlowRelationUtilitites.getFlowPositionFromElement( element, countBCs, ++i );
          bc.setPosition( GeometryFactory.createGM_Point( position.getX(), position.getY(), crs ) );
          m_flowRelationshipsModelChanges.add( bc.getFeature() );
        }
      }
    }
    else
    {
      boundaryCondition.setPosition( GeometryFactory.createGM_Point( FlowRelationUtilitites.getFlowPositionFromElement( element ), crs ) );
      m_flowRelationshipsModelChanges.add( boundaryCondition.getFeature() );
    }
    m_flowRelationshipsModelChanges.add( feature );
  }

  @SuppressWarnings("unchecked")
  private void processNodes( )
  {
    while( m_nodesAdded )
    {
      m_nodesAdded = false;
      for( final Feature feature : m_nodeList )
      {
        final IFE1D2DNode node = (IFE1D2DNode) feature.getAdapter( IFE1D2DNode.class );
        final IFeatureWrapperCollection<IFeatureWrapper2> containers = node.getContainers();

        for( final IFeatureWrapper2 container : containers )
        {
          if( container instanceof IFELine )
          {
            final IFELine line = (IFELine) container;
            try
            {
              // TODO: is there really no other way? Better would be just to call invalidate() on the line-feature

              line.recalculateElementGeometry();
            }
            catch( final GM_Exception e )
            {
              // TODO Auto-generated catch block
              e.printStackTrace();
            }
          }
          else if( container instanceof IFE1D2DEdge )
          {
            final IFE1D2DEdge edge = (IFE1D2DEdge) container;
            final IFeatureWrapperCollection<IFeatureWrapper2> edgeContainers = edge.getContainers();
            for( final IFeatureWrapper2 edgeContainer : edgeContainers )
              addToDiscretisationModelChanges( container );
          }
          addToDiscretisationModelChanges( container );
        }
      }
    }
  }

  public void fireChanges( )
  {
    processNodes();
    if( !m_discretisationModelChanges.isEmpty() )
    {
      final GMLWorkspace workspace = m_discretisationModelChanges.get( 0 ).getWorkspace();
      final Feature[] affectedFeatureList = m_discretisationModelChanges.toArray( new Feature[m_discretisationModelChanges.size()] );
      workspace.fireModellEvent( new FeaturesChangedModellEvent( workspace, affectedFeatureList ) );
    }
    if( !m_flowRelationshipsModelChanges.isEmpty() )
    {
      final GMLWorkspace workspace = m_flowRelationshipsModelChanges.get( 0 ).getWorkspace();
      final Feature[] affectedFeatureList = m_flowRelationshipsModelChanges.toArray( new Feature[m_flowRelationshipsModelChanges.size()] );
      workspace.fireModellEvent( new FeaturesChangedModellEvent( workspace, affectedFeatureList ) );
    }
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    final ICaseDataProvider<IFeatureWrapper2> modelProvider = (ICaseDataProvider<IFeatureWrapper2>) context.getVariable( CaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
    try
    {
      if( !m_flowRelationshipsModelChanges.isEmpty() )
        ((ICommandPoster) modelProvider).postCommand( IFlowRelationshipModel.class, new EmptyCommand( "Get dirty!", false ) ); //$NON-NLS-1$
      // ((ICommandPoster) modelProvider).postCommand( IOperationalModel.class, new EmptyCommand( "Get dirty!", false )
      // ); //$NON-NLS-1$
      // ((ICommandPoster) modelProvider).postCommand( IOperationalModel1D2D.class, new EmptyCommand( "Get dirty!",
      // false ) ); //$NON-NLS-1$
    }
    catch( final Exception e )
    {
      // TODO: handle exception
      e.printStackTrace();
    }
  }
}
