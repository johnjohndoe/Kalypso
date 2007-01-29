/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.kalypsomodel1d2d.ui.map;

import java.awt.Graphics;
import java.awt.Point;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;

import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2DDiscretisationModel;
import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2D_2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.CompositeCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * This class is used to build new 1D2D Elements
 * 
 * @author Gernot Belger
 */
public class ElementGeometryBuilder
{
  /**
   * Stores the count of points which this geometry must have. If it is 0, there is no rule.
   */
  private final int m_cnt_points;

  /** A list of either nodes or locations for new nodes. */
  private final List<Object> m_nodes = new ArrayList<Object>();

  private final IKalypsoFeatureTheme m_nodeTheme;

  /**
   * The constructor.
   * 
   * @param cnt_points
   *          If >0 the the geometry will be finished, if the count of points is reached. If 0 no rule regarding the
   *          count of the points will apply.
   * @param targetCrs
   *          The target coordinate system.
   */
  public ElementGeometryBuilder( final int cnt_points, final IKalypsoFeatureTheme nodeTheme )
  {
    m_nodeTheme = nodeTheme;
    if( cnt_points > 0 )
      m_cnt_points = cnt_points;
    else
      m_cnt_points = 0;
  }

  public ICommand addNode( final Object node ) throws Exception
  {
    if( node instanceof GM_Point || node instanceof FE1D2DNode )
    {
      m_nodes.add( node );

      if( m_nodes.size() == m_cnt_points )
        return finish();
      return null;
    }
    else
      throw new IllegalArgumentException( "node must be either a FE1D2DNode or a GM_Point" );
  }

  /**
   * @see org.kalypso.informdss.manager.util.widgets.IGeometryBuilder#finish()
   */
  public ICommand finish( ) throws Exception
  {
    final CompositeCommand command = new CompositeCommand( "FE1D2D Element hinzufügen" );

    final CommandableWorkspace workspace = m_nodeTheme.getWorkspace();
    final FeatureList featureList = m_nodeTheme.getFeatureList();
    final Feature parentFeature = featureList.getParentFeature();
    final IFeatureType parentType = parentFeature.getFeatureType();
    final IRelationType parentNodeProperty = featureList.getParentFeatureTypeProperty();
    final IRelationType parentEdgeProperty = 
          (IRelationType) parentType.getProperty( 
              Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGES
              /*FE1D2DDiscretisationModel.QNAME_PROP_EDGES*/ );
    final IRelationType parentElementProperty = 
        (IRelationType) parentType.getProperty( 
            Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENTS
            /*FE1D2DDiscretisationModel.QNAME_PROP_ELEMENTS*/ );
    final IGMLSchema schema= workspace.getGMLSchema();
    
    
    final IFeatureType nodeFeatureType=
        schema.getFeatureType( 
            Kalypso1D2DSchemaConstants.WB1D2D_F_NODE);
    final IPropertyType nodeContainerPT=
            nodeFeatureType.getProperty( 
                  Kalypso1D2DSchemaConstants.WB1D2D_PROP_NODE_CONTAINERS );
    
    final IFeatureType edgeFeatureType=
      schema.getFeatureType( 
          Kalypso1D2DSchemaConstants.WB1D2D_F_EDGE);
    final IPropertyType edgeContainerPT=
          edgeFeatureType.getProperty( 
                Kalypso1D2DSchemaConstants.WB1D2D_PROP_EDGE_CONTAINERS );
  
    
    /* Initialize elements needed for edges and elements */
    final FE1D2DDiscretisationModel discModel = 
                new FE1D2DDiscretisationModel( parentFeature );
    List<FeatureChange> changes= new ArrayList<FeatureChange>();
    /* Build new nodes */
    final FE1D2DNode[] nodes = new FE1D2DNode[m_nodes.size()];
    for( int i = 0; i < m_nodes.size(); i++ )
    {
      final Object node = m_nodes.get( i );
      if( node instanceof FE1D2DNode )
        nodes[i] = (FE1D2DNode) node;
      else
      {
        // create new node
        final FE1D2DNode newNode = FE1D2DNode.createNode( discModel );
        newNode.setPoint( (GM_Point) node );
        newNode.setName( "" );
        newNode.setDescription( "manuell digitalisiert" );
        final AddFeatureCommand addNodeCommand = new AddFeatureCommand( workspace, parentFeature, parentNodeProperty, -1, newNode.getFeature(), null, false );
        command.addCommand( addNodeCommand );
        nodes[i] = newNode;
      }
    }

    /* Build new edges */
    final FE1D2DEdge[] edges = new FE1D2DEdge[m_nodes.size()];
    final boolean[] edgesGen = new boolean[m_nodes.size()]; // flag indicatig if edge was generated
    for( int i = 0; i < edges.length; i++ )
    {
      final FE1D2DNode node0 = nodes[i];
      final FE1D2DNode node1 = nodes[(i + 1) % nodes.length];

      /* Search for existing edge */
      final FE1D2DEdge edge = discModel.findEdge( node0, node1 );
      if( edge == null )
      {
        final FE1D2DEdge newEdge = FE1D2DEdge.createEdge( discModel );
        newEdge.setNodes( node0, node1 );
        edges[i] = newEdge;
        edgesGen[i] = true;
        final AddFeatureCommand addEdgeCommand = 
                  new AddFeatureCommand( 
                              workspace, 
                              parentFeature, 
                              parentEdgeProperty, 
                              -1, 
                              newEdge.getFeature(), null, false );
        
        command.addCommand( addEdgeCommand );
        
        addNodeContainerCommand( 
                      workspace, 
                      node0, 
                      node1, 
                      nodeContainerPT, 
                      newEdge,
                      changes);
      }
      else
      {
        edges[i] = edge;
        edgesGen[i] = false;
      }
    }
   
    /* Build new element */
    final FE1D2D_2DElement element = 
          FE1D2D_2DElement.createPolyElement( discModel );

    element.setEdges( edges );

    final AddFeatureCommand addElementCommand = 
              new AddFeatureCommand( 
                    workspace, 
                    parentFeature, 
                    parentElementProperty, 
                    -1, 
                    element.getFeature(), 
                    null, 
                    true );
    command.addCommand( addElementCommand );
     
    addEdgeContainerCommand( 
                  workspace, 
                  edges, 
                  edgeContainerPT, 
                  element,
                  changes);
    command.addCommand( 
        new ListPropertyChangeCommand(
            workspace,changes.toArray( new FeatureChange[changes.size()] )) );
    return command;
  }

  static final void addEdgeContainerCommand(
      GMLWorkspace workspace,
      IFE1D2DEdge[] edges,
      IPropertyType propertyType,
      IFE1D2DElement element,
      List<FeatureChange> changes)
  {
    Feature elementFeature=element.getWrappedFeature();
    String elementID=elementFeature.getId();
    
//    FeatureChange changes[]= new FeatureChange[edges.length];
    for(int i=0;i<edges.length;i++)
    {
      changes.add( 
          new FeatureChange(
              edges[i].getWrappedFeature(),
              propertyType,
              elementID));
    }
    
//    ChangeFeaturesCommand changeCommand=
//      new ChangeFeaturesCommand(
//                workspace,
//                changes);
//    return changeCommand;
  }
  static final void addNodeContainerCommand(
                                                  GMLWorkspace workspace,
                                                  IFE1D2DNode node0,
                                                  IFE1D2DNode node1,
                                                  IPropertyType propertyType,
                                                  IFE1D2DEdge edge,
                                                  List<FeatureChange> changes)
  {
    Feature edgeFeature=edge.getWrappedFeature();
    Feature node0Feature=node0.getWrappedFeature();
    Feature node1Feature=node1.getWrappedFeature();
    String edgeID=edgeFeature.getId();
    FeatureChange change0= 
            new FeatureChange(
                node0Feature,
                propertyType,
                edgeID);
    changes.add( change0 );
    FeatureChange change1= 
      new FeatureChange(
          node1Feature,
          propertyType,
          edgeID);
    changes.add( change1 );
//    ChangeFeaturesCommand changeCommand=
//        new ChangeFeaturesCommand(
//                  workspace,
//                  new FeatureChange[]{change0,change1});
//    return changeCommand;
  }
  /**
   * @see org.kalypso.informdss.manager.util.widgets.IGeometryBuilder#paint(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform)
   */
  public void paint( final Graphics g, final GeoTransform projection, final Point currentPoint )
  {
    // IMPORTANT: we remeber GM_Points (not Point's) and retransform them for painting
    // because the projection depends on the current map-extent, so this builder
    // is stable in regard to zoom in/out
    if( !m_nodes.isEmpty() )
    {
      final int[][] points = getPointArrays( projection, currentPoint );

      final int[] arrayX = points[0];
      final int[] arrayY = points[1];

      /* Paint a linestring. */
      g.drawPolygon( arrayX, arrayY, arrayX.length );
      drawHandles( g, arrayX, arrayY );
    }
  }

  private int[][] getPointArrays( final GeoTransform projection, final Point currentPoint )
  {
    final List<Integer> xArray = new ArrayList<Integer>();
    final List<Integer> yArray = new ArrayList<Integer>();

    for( final Object node : m_nodes )
    {
      final GM_Point point;
      if( node instanceof FE1D2DNode )
        point = ((FE1D2DNode) node).getPoint();
      else
        point = (GM_Point) node;

      final int x = (int) projection.getDestX( point.getX() );
      final int y = (int) projection.getDestY( point.getY() );

      xArray.add( new Integer( x ) );
      yArray.add( new Integer( y ) );
    }

    xArray.add( currentPoint.x );
    yArray.add( currentPoint.y );

    final int[] xs = ArrayUtils.toPrimitive( xArray.toArray( new Integer[xArray.size()] ) );
    final int[] ys = ArrayUtils.toPrimitive( yArray.toArray( new Integer[yArray.size()] ) );

    return new int[][] { xs, ys };
  }

  private void drawHandles( final Graphics g, final int[] x, final int[] y )
  {
    int sizeOuter = 6;
    for( int i = 0; i < y.length; i++ )
      g.drawRect( x[i] - sizeOuter / 2, y[i] - sizeOuter / 2, sizeOuter, sizeOuter );
  }
}
