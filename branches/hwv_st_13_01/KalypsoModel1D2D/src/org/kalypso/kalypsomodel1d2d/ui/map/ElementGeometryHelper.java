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
package org.kalypso.kalypsomodel1d2d.ui.map;

import java.awt.Point;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.Element1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.PolyElement;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ListPropertyChangeCommand;
import org.kalypso.ogc.gml.command.CompositeCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Ring;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.algorithm.CGAlgorithms;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author Thomas Jung
 */
public class ElementGeometryHelper
{

  /**
   * distance for searching of already existing {@link IFE1D2DNode}s TODO: should be specified in kalypso preferences
   * page
   */
  private static final double SEARCH_DISTANCE = 0.1;

  private static final DateFormat m_DF = new SimpleDateFormat( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.NodalBCSelectionWizard.0" ) ); //$NON-NLS-1$

  // /**
  // * wrapper for {@link createAdd2dElement( final CompositeCommand command, final CommandableWorkspace workspace,
  // final Feature parentFeature, final IFEDiscretisationModel1d2d discModel, final List<GM_Point> points, final int
  // pIntPointsCount ) }
  // * with additional points counter, 0 means not bounded.
  // */
  // public static IFeatureWrapper2 createAdd2dElement( final CompositeCommand command, final CommandableWorkspace
  // workspace, final Feature parentFeature, final IFEDiscretisationModel1d2d discModel, final List<GM_Point> points )
  // {
  // return createAdd2dElement( command, workspace, parentFeature, discModel, points, points.size() );
  // }
  /**
   * Fills an {@link org.kalypso.kalypsomodel1d2d.ui.map.cmds.AddElementCommand} in a given {@link CompositeCommand}<br>
   * The new {@link IFE1D2DElement} is specified by its geometry.
   * 
   * @param command
   *          the {@link CompositeCommand} to be filled
   * @param workspace
   * @param parentFeature
   * @param parentNodeProperty
   * @param parentEdgeProperty
   * @param parentElementProperty
   * @param nodeContainerPT
   * @param edgeContainerPT
   * @param discModel
   *          the discretisation model
   * @param points
   *          the {@link GM_Point} list
   */
  public static IFeatureWrapper2 createAdd2dElement( final CompositeCommand command, final CommandableWorkspace workspace, final IFEDiscretisationModel1d2d discModel, final List<GM_Point> points )

  {
    final IGMLSchema schema = workspace.getGMLSchema();

    final IFeatureType nodeFeatureType = schema.getFeatureType( Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );

    final IPropertyType nodeContainerPT = nodeFeatureType.getProperty( IFE1D2DNode.WB1D2D_PROP_NODE_CONTAINERS );

    final IFeatureType edgeFeatureType = schema.getFeatureType( IFE1D2DEdge.QNAME );
    final IPropertyType edgeContainerPT = edgeFeatureType.getProperty( IFE1D2DEdge.WB1D2D_PROP_EDGE_CONTAINERS );

    final Feature parentFeature = discModel.getFeature();
    final IFeatureType parentType = parentFeature.getFeatureType();

    final IRelationType parentNodeProperty = (IRelationType) parentType.getProperty( IFEDiscretisationModel1d2d.WB1D2D_PROP_NODES );
    final IRelationType parentEdgeProperty = (IRelationType) parentType.getProperty( IFEDiscretisationModel1d2d.WB1D2D_PROP_EDGES );
    final IRelationType parentElementProperty = (IRelationType) parentType.getProperty( IFEDiscretisationModel1d2d.WB1D2D_PROP_ELEMENTS );

    final List<FeatureChange> changes = new ArrayList<FeatureChange>();

    /* Build new nodes */
    final IFE1D2DNode[] nodes = buildNewNodes( points, command, workspace, parentFeature, parentNodeProperty, discModel, SEARCH_DISTANCE );

    /* Build new edges */
    final IFE1D2DEdge[] edges = buildNewEdges( points.size(), command, workspace, parentFeature, parentEdgeProperty, nodeContainerPT, discModel, changes, nodes );

    /* Build new element */
    IFE1D2DElement newElement = null;
    newElement = PolyElement.createPolyElement( discModel );
    ((PolyElement) newElement).setEdges( edges );

    final AddFeatureCommand addElementCommand = new AddFeatureCommand( workspace, parentFeature, parentElementProperty, -1, newElement.getFeature(), null, true );
    command.addCommand( addElementCommand );

    addEdgeContainerCommand( edges, edgeContainerPT, newElement, changes );

    command.addCommand( new ListPropertyChangeCommand( workspace, changes.toArray( new FeatureChange[changes.size()] ) ) );

    return newElement;
  }

  /**
   * Fills an {@link org.kalypso.kalypsomodel1d2d.ui.map.cmds.AddElementCommand} in a given {@link CompositeCommand}<br>
   * The new {@link IFE1D2DElement} is specified by its geometry.
   * 
   * @param command
   *          the {@link CompositeCommand} to be filled
   * @param workspace
   * @param parentFeature
   * @param nodeContainerPT
   * @param discModel
   *          the discretisation model
   * @param points
   *          the {@link GM_Point} list
   */
  @SuppressWarnings("unchecked")
  public static void createAdd1dElement( final CompositeCommand command, final CommandableWorkspace workspace, final Feature parentFeature, final IFEDiscretisationModel1d2d discModel, final List<GM_Point> points )
  {
    final IGMLSchema schema = workspace.getGMLSchema();

    final IFeatureType nodeFeatureType = schema.getFeatureType( Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
    final IPropertyType nodeContainerPT = nodeFeatureType.getProperty( IFE1D2DNode.WB1D2D_PROP_NODE_CONTAINERS );

    final IFeatureType parentType = parentFeature.getFeatureType();

    final IRelationType parentNodeProperty = (IRelationType) parentType.getProperty( IFEDiscretisationModel1d2d.WB1D2D_PROP_NODES );
    final IRelationType parentEdgeProperty = (IRelationType) parentType.getProperty( IFEDiscretisationModel1d2d.WB1D2D_PROP_EDGES );
    final IRelationType parentElementProperty = (IRelationType) parentType.getProperty( IFEDiscretisationModel1d2d.WB1D2D_PROP_ELEMENTS );

    final List<FeatureChange> changes = new ArrayList<FeatureChange>();

    /* Build new nodes */
    final IFE1D2DNode[] nodes = buildNewNodes( points, command, workspace, parentFeature, parentNodeProperty, discModel, SEARCH_DISTANCE );

    /* Build one new edge for the 1d element */
    final IFE1D2DEdge[] edges = buildNewEdges( 1, command, workspace, parentFeature, parentEdgeProperty, nodeContainerPT, discModel, changes, nodes );

    /* Build new element */
    final IFeatureType parentFT = parentFeature.getFeatureType();
    final IFeatureType lElement1DType = parentFT.getGMLSchema().getFeatureType( IElement1D.QNAME );

    final Feature eleFeature = parentFeature.getWorkspace().createFeature( parentFeature, parentElementProperty, lElement1DType );

    final IElement1D newElement = new Element1D( eleFeature );
    newElement.setEdge( edges[0] );
    // final IElement1D newElement = ModelOps.createElement1d( discModel, edges[0] );

    final AddFeatureCommand addElementCommand = new AddFeatureCommand( workspace, parentFeature, parentElementProperty, -1, newElement.getFeature(), null, true );
    command.addCommand( addElementCommand );

    command.addCommand( new ListPropertyChangeCommand( workspace, changes.toArray( new FeatureChange[changes.size()] ) ) );
  }

  public static final void addNodeContainerCommand( final IFE1D2DNode node0, final IFE1D2DNode node1, final IPropertyType propertyType, final IFE1D2DEdge edge, final List<FeatureChange> changes )
  {
    final Feature edgeFeature = edge.getFeature();
    final Feature node0Feature = node0.getFeature();
    final Feature node1Feature = node1.getFeature();
    final String edgeID = edgeFeature.getId();
    final FeatureChange change0 = new FeatureChange( node0Feature, propertyType, edgeID );
    changes.add( change0 );
    final FeatureChange change1 = new FeatureChange( node1Feature, propertyType, edgeID );
    changes.add( change1 );
  }

  public static final void addEdgeContainerCommand( final IFE1D2DEdge[] edges, final IPropertyType propertyType, final IFE1D2DElement element, final List<FeatureChange> changes )
  {
    final Feature elementFeature = element.getFeature();
    final String elementID = elementFeature.getId();

    // FeatureChange changes[]= new FeatureChange[edges.length];

    for( final IFE1D2DEdge element2 : edges )
    {
      changes.add( new FeatureChange( element2.getFeature(), propertyType, elementID ) );
    }
  }

  /**
   * Creates new {@link IFE1D2DNode}s specified by their geometry ( {@link GM_Point} list) and fills a given
   * {@link CompositeCommand} with the {@link AddFeatureCommand}s for the new nodes. Via a given search distance it is
   * checked, if there are already existing {@link IFE1D2DNode}s in the neighborhood. If this is the case,no new nodes
   * will be generated.
   * 
   * @param points
   *          the points
   * @param command
   * @param workspace
   * @param parentFeature
   * @param parentNodeProperty
   * @param discModel
   * @param searchDistance
   */
  public static IFE1D2DNode[] buildNewNodes( final List<GM_Point> points, final CompositeCommand command, final CommandableWorkspace workspace, final Feature parentFeature, final IRelationType parentNodeProperty, final IFEDiscretisationModel1d2d discModel, final double searchDistance )
  {
    /* Build new nodes */
    final IFE1D2DNode[] nodes = new IFE1D2DNode[points.size()];
    for( int i = 0; i < points.size(); i++ )
    {
      final GM_Point point = points.get( i );

      // check, if there is already a node at that position
      final IFE1D2DNode foundNode = discModel.findNode( point, searchDistance );

      if( foundNode == null )
      {
        // create new node
        final FE1D2DNode newNode = FE1D2DNode.createNode( discModel );
        newNode.setPoint( point );
        newNode.setName( "" ); //$NON-NLS-1$
        //        newNode.setDescription( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryBuilder.3" ) ); //$NON-NLS-1$
        newNode.setDescription( m_DF.format( new Date() ) ); //$NON-NLS-1$
        final AddFeatureCommand addNodeCommand = new AddFeatureCommand( workspace, parentFeature, parentNodeProperty, -1, newNode.getFeature(), null, false );
        command.addCommand( addNodeCommand );// TODO: why not put into changes?
        nodes[i] = newNode;
      }
      else
        nodes[i] = foundNode;
    }

    // reverse direction of element nodes if not in ccw-order
    final Coordinate[] jtsCoordinates = new Coordinate[nodes.length + 1];
    for( int i = 0; i < nodes.length; i++ )
    {
      jtsCoordinates[i] = JTSAdapter.export( nodes[i].getPoint().getPosition() );
    }
    jtsCoordinates[nodes.length] = jtsCoordinates[0];
    if( CGAlgorithms.isCCW( jtsCoordinates ) )
    {
      final List<IFE1D2DNode> nodeList = Arrays.asList( nodes );
      Collections.reverse( nodeList );
      nodeList.toArray( nodes );
    }

    return nodes;
  }

  public static IFE1D2DEdge[] buildNewEdges( final int numOfEdges, final CompositeCommand command, final CommandableWorkspace workspace, final Feature parentFeature, final IRelationType parentEdgeProperty, final IPropertyType nodeContainerPT, final IFEDiscretisationModel1d2d discModel, final List<FeatureChange> changes, final IFE1D2DNode[] nodes )
  {
    /* Build new edges */
    final IFE1D2DEdge[] edges = new IFE1D2DEdge[numOfEdges];
    for( int i = 0; i < edges.length; i++ )
    {
      final IFE1D2DNode node0 = nodes[i];
      final IFE1D2DNode node1 = nodes[(i + 1) % nodes.length];

      /* Search for existing edge */
      final IFE1D2DEdge edge = discModel.findEdge( node0, node1 );
      if( edge == null )
      {
        final FE1D2DEdge newEdge = FE1D2DEdge.createEdge( discModel );
        newEdge.setNodes( node0, node1 );
        edges[i] = newEdge;
        final AddFeatureCommand addEdgeCommand = new AddFeatureCommand( workspace, parentFeature, parentEdgeProperty, -1, newEdge.getFeature(), null, false );

        command.addCommand( addEdgeCommand );

        ElementGeometryHelper.addNodeContainerCommand( node0, node1, nodeContainerPT, newEdge, changes );
      }
      else
        edges[i] = edge;
    }

    return edges;
  }

  /**
   * returns a points array of a polygon specified as an array of {@link GM_Point}
   */
  public static int[][] getPolygonAsPointArrays( final GeoTransform projection, final GM_Point[] nodes )
  {
    final List<Integer> xArray = new ArrayList<Integer>();
    final List<Integer> yArray = new ArrayList<Integer>();

    for( int i = 0; i < nodes.length; i++ )
    {
      final int x = (int) projection.getDestX( nodes[i].getX() );
      final int y = (int) projection.getDestY( nodes[i].getY() );

      xArray.add( x );
      yArray.add( y );

    }

    final int x = (int) projection.getDestX( nodes[0].getX() );
    final int y = (int) projection.getDestY( nodes[0].getY() );

    xArray.add( x );
    yArray.add( y );

    final int[] xs = ArrayUtils.toPrimitive( xArray.toArray( new Integer[xArray.size()] ) );
    final int[] ys = ArrayUtils.toPrimitive( yArray.toArray( new Integer[yArray.size()] ) );

    return new int[][] { xs, ys };
  }

  /**
   * returns a points array of a line specified by two {@link GM_Point}
   */
  public static int[][] getLineAsPointArrays( final GeoTransform projection, final GM_Point node1, final Point currentPos )
  {
    final List<Integer> xArray = new ArrayList<Integer>();
    final List<Integer> yArray = new ArrayList<Integer>();

    final int x = (int) projection.getDestX( node1.getX() );
    final int y = (int) projection.getDestY( node1.getY() );

    xArray.add( x );
    yArray.add( y );

    xArray.add( currentPos.x );
    yArray.add( currentPos.y );

    final int[] xs = ArrayUtils.toPrimitive( xArray.toArray( new Integer[xArray.size()] ) );
    final int[] ys = ArrayUtils.toPrimitive( yArray.toArray( new Integer[yArray.size()] ) );

    return new int[][] { xs, ys };
  }

  /**
   * converts a {@link GM_Point} array into a {@link GM_Position} array.
   */
  public static GM_Position[] linePositionsFromNodes( final GM_Point[] nodes )
  {
    final GM_Position[] poses = new GM_Position[nodes.length];
    for( int i = 0; i < nodes.length; i++ )
    {
      poses[i] = nodes[i].getPosition();
    }

    return poses;
  }

  /**
   * converts a {@link GM_Point} array into a {@link GM_Position} array and closes it by adding the first point at the
   * end.
   */
  public static GM_Position[] ringPositionsFromNodes( final GM_Point[] nodes )
  {
    final GM_Position[] poses = new GM_Position[nodes.length + 1];
    for( int i = 0; i < nodes.length; i++ )
    {
      poses[i] = nodes[i].getPosition();
    }
    // close the ring
    poses[nodes.length] = poses[0];

    return poses;
  }

  public static void createFE1D2DfromSurface( final CommandableWorkspace workspace, final IFEDiscretisationModel1d2d discModel, final GM_Surface<GM_SurfacePatch> surface ) throws Exception
  {
    final String crs = surface.getCoordinateSystem();
    for( final GM_SurfacePatch surfacePatch : surface )
    {
      final GM_Position[] poses = surfacePatch.getExteriorRing();
      createFE1D2DfromPositions( workspace, discModel, poses, crs );
    }
  }

  public static void createFE1D2DfromRing( final CommandableWorkspace workspace, final IFEDiscretisationModel1d2d discModel, final GM_Ring ring ) throws Exception
  {
    final GM_Position[] poses = ring.getPositions();
    final String crs = ring.getCoordinateSystem();
    createFE1D2DfromPositions( workspace, discModel, poses, crs );
  }

  public static void createFE1D2DfromPositions( final CommandableWorkspace workspace, final IFEDiscretisationModel1d2d discModel, final GM_Position[] poses, final String crs ) throws Exception
  {
    final CompositeCommand command = new CompositeCommand( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryBuilder.1" ) ); //$NON-NLS-1$

    // create the nodes
    final List<GM_Point> nodes = new ArrayList<GM_Point>();

    // TODO: handle snapping on neigboring nodes => quadrangles to triangles
    for( int i = 0; i < poses.length - 1; i++ )
      nodes.add( org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Point( poses[i], crs ) );

    // create the new elements
    if( nodes.size() == 3 || nodes.size() == 4 )
    {
      createAdd2dElement( command, workspace, discModel, nodes );

      // inside the loop because we want to avoid duplicates
      workspace.postCommand( command );
    }
  }
}
