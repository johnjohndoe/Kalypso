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
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.ArrayUtils;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.element1d.Create2dElementCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_AbstractSurfacePatch;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.algorithm.CGAlgorithms;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author Thomas Jung
 */
public class ElementGeometryHelper
{

  /**
   * Creates new {@link IFE1D2DNode}s specified by their geometry {@link GM_Point} <br/>
   * Via a given search distance it is checked, if there are already existing {@link IFE1D2DNode}s in the neighborhood.
   * If this is the case,no new nodes will be generated.
   * 
   * @param discModel
   * @param points
   *          the points
   */
  public static IFE1D2DNode[] buildNewNodes( final IFEDiscretisationModel1d2d discModel, final GM_Point... points )
  {
    /* Build new nodes */
    final IFE1D2DNode[] nodes = new IFE1D2DNode[points.length];
    for( int i = 0; i < points.length; i++ )
    {
      final GM_Point point = points[i];
      nodes[i] = discModel.createNode( point );
    }
    return nodes;
  }

  /**
   * Makes sure that the given ring of nodes is oriented clockwise.
   */
  public static IFE1D2DNode[] makeCCW( final IFE1D2DNode[] nodes )
  {
    // reverse direction of element nodes if not in ccw-order
    final Coordinate[] jtsCoordinates = new Coordinate[nodes.length + 1];
    for( int i = 0; i < nodes.length; i++ )
      jtsCoordinates[i] = JTSAdapter.export( nodes[i].getPoint().getPosition() );
    jtsCoordinates[nodes.length] = jtsCoordinates[0];

    if( CGAlgorithms.isCCW( jtsCoordinates ) )
      ArrayUtils.reverse( nodes );

    return nodes;
  }

  public static IFE1D2DEdge[] buildNewEdges( final IFEDiscretisationModel1d2d discModel, final IFE1D2DNode[] nodes, final int numOfEdges )
  {
    /* Build new edges */
    final IFE1D2DEdge[] edges = new IFE1D2DEdge[numOfEdges];
    for( int i = 0; i < numOfEdges; i++ )
    {
      final IFE1D2DNode node0 = nodes[i];
      final IFE1D2DNode node1 = nodes[(i + 1) % nodes.length];
      edges[i] = discModel.createEdge( node0, node1 );
    }

    return edges;
  }

  /**
   * returns a points array of a polygon specified as an array of {@link GM_Point}
   */
  public static int[][] getPolygonAsPointArrays( final GeoTransform projection, final GM_Point[] nodes )
  {
    final List<Integer> xArray = new ArrayList<>();
    final List<Integer> yArray = new ArrayList<>();

    for( final GM_Point node : nodes )
    {
      final int x = (int)projection.getDestX( node.getX() );
      final int y = (int)projection.getDestY( node.getY() );

      xArray.add( x );
      yArray.add( y );

    }

    final int x = (int)projection.getDestX( nodes[0].getX() );
    final int y = (int)projection.getDestY( nodes[0].getY() );

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
    final List<Integer> xArray = new ArrayList<>();
    final List<Integer> yArray = new ArrayList<>();

    final int x = (int)projection.getDestX( node1.getX() );
    final int y = (int)projection.getDestY( node1.getY() );

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

  public static void createFE1D2DfromSurface( final CommandableWorkspace workspace, final IFEDiscretisationModel1d2d discModel, final GM_Polygon surface ) throws Exception
  {
    final String crs = surface.getCoordinateSystem();
    for( final GM_AbstractSurfacePatch surfacePatch : surface )
    {
      final GM_Position[] poses = surfacePatch.getExteriorRing();
      createFE1D2DfromPositions( workspace, discModel, poses, crs );
    }
  }

  public static void createFE1D2DfromPositions( final CommandableWorkspace workspace, final IFEDiscretisationModel1d2d discModel, final GM_Position[] poses, final String crs ) throws Exception
  {
    // create the nodes
    final GM_Point[] nodes = new GM_Point[poses.length - 1];

    // TODO: handle snapping on neighboring nodes => quadrangles to triangles
    for( int i = 0; i < poses.length - 1; i++ )
      nodes[i] = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Point( poses[i], crs );

    // create the new elements
    if( nodes.length == 3 || nodes.length == 4 )
    {
      final Create2dElementCommand command = new Create2dElementCommand( discModel, nodes );
      workspace.postCommand( command );
    }
  }
}
