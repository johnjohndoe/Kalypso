/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.vecmath.Vector2d;

import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Calculates a continuity line from a list of nodes.
 * 
 * @author Gernot Belger
 */
public class ContinuityLine2DGeometryBuilder
{
  private final List<IFE1D2DNode> m_continuityNodes = new ArrayList<>();

  public ContinuityLine2DGeometryBuilder( final IFE1D2DNode[] nodes, final IProgressMonitor monitor )
  {
    recalculateContinuityLine( nodes, monitor );
  }

  public GM_Curve getCurve( )
  {
    final IFE1D2DNode[] nodes = getContinuityNodes();
    if( nodes.length < 2 )
      return null;

    final GM_Position[] nodePositions = new GM_Position[nodes.length];
    for( int i = 0; i < nodePositions.length; i++ )
      nodePositions[i] = nodes[i].getPoint().getPosition();

    try
    {
      final String crs = nodes[0].getPoint().getCoordinateSystem();
      if( crs == null )
        throw new IllegalStateException();

      return GeometryFactory.createGM_Curve( nodePositions, crs );
    }
    catch( final GM_Exception e )
    {
      throw new IllegalArgumentException( e );
    }
  }

  public IFE1D2DNode[] getContinuityNodes( )
  {
    return m_continuityNodes.toArray( new IFE1D2DNode[m_continuityNodes.size()] );
  }

  private void recalculateContinuityLine( final IFE1D2DNode[] nodes, final IProgressMonitor monitor )
  {
    for( int i = 0; i < nodes.length - 1; i++ )
    {
      final IFE1D2DNode startNode = nodes[i];
      final IFE1D2DNode endNode = nodes[i + 1];

      if( startNode != endNode )
      {
        final IFE1D2DNode[] path = calculatePath( startNode, endNode, monitor );
        if( path == null )
        {
          /* one part of the conti line is not connected, the whole line is hence invalid. */
          m_continuityNodes.clear();
          return;
        }

        addContinuityNodes( path );
      }
    }
  }

  private IFE1D2DNode[] calculatePath( /* final WeightedGraph<IFE1D2DNode, IFE1D2DEdge> discGraph, */final IFE1D2DNode startNode, final IFE1D2DNode endNode, final IProgressMonitor monitor )
  {
    if( startNode.isAdjacentNode( endNode ) )
      return new IFE1D2DNode[] { startNode, endNode };

//    if( discGraph == null )
    return calculateOwnPath( startNode, endNode, monitor );
//    else
//      return calculatePathFromGraph( discGraph, startNode, endNode );
  }

  private IFE1D2DNode[] calculateOwnPath( final IFE1D2DNode startNode, final IFE1D2DNode endNode, final IProgressMonitor monitor )
  {
    final Collection<IFE1D2DNode> path = new ArrayList<>();

    path.add( startNode );

    IFE1D2DNode currentNode = startNode;

    // TODO: !!!Potential endless loop!! I once got it (Gernot)
    while( currentNode != endNode )
    {
      currentNode = findNextNode( currentNode, endNode, path, monitor );

      if( currentNode == null )
      {
        // FIXME: check
        return null;
      }

      path.add( currentNode );

      ProgressUtilities.worked( monitor, 1 );
    }

    return path.toArray( new IFE1D2DNode[path.size()] );
  }

  private IFE1D2DNode findNextNode( final IFE1D2DNode currentNode, final IFE1D2DNode endNode, final Collection<IFE1D2DNode> path, final IProgressMonitor monitor )
  {
    final Collection<IFE1D2DNode> neighbourNodes = currentNode.getAdjacentNodes();

    final Vector2d currentDirection = nodesDirection( currentNode, endNode );

    // REMARK: using the best direction instead of search by distance yealds better results in pratice

    // IFE1D2DNode nearestNode = null;
    IFE1D2DNode straightestNode = null;
    double shortestDistance = Double.MAX_VALUE;
    double smallestAngle = Double.MAX_VALUE;

    for( final IFE1D2DNode node : neighbourNodes )
    {
      if( node == endNode )
        return node;

      if( path.contains( node ) )
        continue;

      final Vector2d direction = nodesDirection( currentNode, node );

      final double nodesDistance = nodesDistance( node, endNode );

      /* calculate angle between both directions */
      double nodesAngle = Math.atan2( direction.y, direction.x ) - Math.atan2( currentDirection.y, currentDirection.x );
      if( nodesAngle > Math.PI )
        nodesAngle -= Math.PI * 2;
      else if( nodesAngle < -Math.PI )
        nodesAngle += Math.PI * 2;
      nodesAngle = Math.abs( nodesAngle );

      if( nodesDistance < shortestDistance )
      {
        shortestDistance = nodesDistance;
        // nearestNode = node;
      }

      if( nodesAngle < smallestAngle )
      {
        smallestAngle = nodesAngle;
        straightestNode = node;
      }

      ProgressUtilities.worked( monitor, 1 );
    }

    return straightestNode;
//    return nearestNode;
  }

  private Vector2d nodesDirection( final IFE1D2DNode node1, final IFE1D2DNode node2 )
  {
    final GM_Point p1 = node1.getPoint();
    final GM_Point p2 = node2.getPoint();

    final Vector2d v = new Vector2d( p1.getX() - p2.getX(), p1.getY() - p2.getY() );
    // v.normalize();
    return v;
  }

//  private IFE1D2DNode[] calculatePathFromGraph( final WeightedGraph<IFE1D2DNode, IFE1D2DEdge> discGraph, final IFE1D2DNode startNode, final IFE1D2DNode endNode )
//  {
//    final List<IFE1D2DEdge> path = BellmanFordShortestPath.findPathBetween( discGraph, startNode, endNode );
//    if( path == null || path.size() == 0 )
//      return null;
//
//    /* extract nodes from path, edges are not directed in direction of path */
//    IFE1D2DNode lastNode = startNode;
//
//    final Collection<IFE1D2DNode> nodes = new ArrayList<>();
//    nodes.add( startNode );
//
//    for( final IFE1D2DEdge edge : path )
//    {
//      final IFE1D2DNode[] edgeNodes = edge.getNodes();
//
//      final IFE1D2DNode node1 = edgeNodes[0];
//      final IFE1D2DNode node2 = edgeNodes[1];
//
//      if( node1 == lastNode )
//      {
//        nodes.add( node2 );
//        lastNode = node2;
//      }
//      else if( node2 == lastNode )
//      {
//        nodes.add( node1 );
//        lastNode = node1;
//      }
//      else
//        throw new IllegalStateException();
//    }
//
//    return nodes.toArray( new IFE1D2DNode[nodes.size()] );
//  }

  private void addContinuityNodes( final IFE1D2DNode[] nodes )
  {
    for( final IFE1D2DNode node : nodes )
    {
      /* avoid adding duplicate nodes */
      final int size = m_continuityNodes.size();
      if( size == 0 || m_continuityNodes.get( size - 1 ) != node )
        m_continuityNodes.add( node );
    }
  }

  private double nodesDistance( final IFE1D2DNode node1, final IFE1D2DNode node2 )
  {
    final GM_Point p1 = node1.getPoint();
    final GM_Point p2 = node2.getPoint();
    return p1.distance( p2 );
  }
}