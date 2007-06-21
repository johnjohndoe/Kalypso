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
package org.kalypso.kalypsomodel1d2d.schema.binding.results;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import org.kalypsodeegree.model.geometry.GM_Point;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Thomas Jung
 * 
 */
public class ElementResult
{
  public boolean isWet = false;

  final private int m_elemID;

  private int m_currentRougthnessClassID;

  private int m_previousRoughnessClassID;

  private int m_eleminationNumber;

  private final HashMap<Integer, ArcResult> m_arcResult = new HashMap<Integer, ArcResult>();

  private final List<GMLNodeResult> m_cornerNodes = new LinkedList<GMLNodeResult>();

  private final List<GMLNodeResult> m_midsideNodes = new LinkedList<GMLNodeResult>();

  private SimpleNodeResult m_centerNode = null;

  public ElementResult( int id, int currentRougthnessClass, int previousRoughnessClass, int eleminationNum )
  {
    m_elemID = id;
    m_currentRougthnessClassID = currentRougthnessClass;
    m_previousRoughnessClassID = previousRoughnessClass;
    m_eleminationNumber = eleminationNum;
  }

  public void setArc( ArcResult arcresult )
  {
    m_arcResult.put( m_arcResult.size(), arcresult );
  }

  public ArcResult getArc( int id )
  {
    return m_arcResult.get( id );
  }

  public int getNumArcs( )
  {
    return m_arcResult.size();
  }

  public int getCurrentRougthnessClassID( )
  {
    return m_currentRougthnessClassID;
  }

  public void setCurrentRougthnessClassID( int currentRougthnessClassID )
  {
    m_currentRougthnessClassID = currentRougthnessClassID;
  }

  public int getPreviousRoughnessClassID( )
  {
    return m_previousRoughnessClassID;
  }

  public void setPreviousRoughnessClassID( int previousRoughnessClassID )
  {
    m_previousRoughnessClassID = previousRoughnessClassID;
  }

  public int getEleminationNumber( )
  {
    return m_eleminationNumber;
  }

  public void setEleminationNumber( int eleminationNumber )
  {
    m_eleminationNumber = eleminationNumber;
  }

  public int getElemID( )
  {
    return m_elemID;
  }

  public INodeResult getCornerNodes( int index )
  {
    return m_cornerNodes.get( index );
  }

  public void setCornerNodes( GMLNodeResult nodeResult )
  {

    m_cornerNodes.add( nodeResult );
  }

  public INodeResult getMidsideNodes( int index )
  {
    return m_midsideNodes.get( index );
  }

  public void setMidsideNodes( GMLNodeResult nodeResult )
  {
    m_midsideNodes.add( nodeResult );
  }

  public int getNumCornerNodes( )
  {
    return m_cornerNodes.size();
  }

  public int getNumMidsideNodes( )
  {
    return m_midsideNodes.size();
  }

  public void createCenterNode( )
  {
    m_centerNode = new SimpleNodeResult();
    interpolateCenterNode();
  }

  /**
   * creates a center node for the element by using the corner and mid-side nodes.
   */
  private void interpolateCenterNode( )
  {
    /* get the center point location by using the corner nodes */
    double sumXCoord = 0;
    double sumYCoord = 0;
    double sumZCoord = 0;
    double sumDepth = 0;
    double sumWaterlevel = 0;
    double sumVxCorner = 0;
    double sumVyCorner = 0;
    double sumVxMid = 0;
    double sumVyMid = 0;

    for( int i = 0; i < m_cornerNodes.size(); i++ )
    {
      /** location */
      final GMLNodeResult node = m_cornerNodes.get( i );
      final GM_Point p = node.getPoint();

      sumXCoord = sumXCoord + p.getX();
      sumYCoord = sumYCoord + p.getY();
      sumZCoord = sumZCoord + p.getZ();

      sumWaterlevel = sumWaterlevel + node.getWaterlevel();
      sumDepth = sumDepth + node.getDepth();
      sumVxCorner = sumVxCorner + node.getVelocity().get( 0 );
      sumVyCorner = sumVyCorner + node.getVelocity().get( 1 );
    }

    final CS_CoordinateSystem crs = m_cornerNodes.get( 0 ).getPoint().getCoordinateSystem();
    final double x = sumXCoord / m_cornerNodes.size();
    final double y = sumYCoord / m_cornerNodes.size();
    final double z = sumZCoord / m_cornerNodes.size();

    m_centerNode.setLocation( x, y, z, crs );

    /* interpolate the data */
    // water level
    final double waterlevel = sumWaterlevel / m_cornerNodes.size();
    m_centerNode.setWaterlevel( waterlevel );

    // depth
    double depth = waterlevel - z;
    List<Double> velocity = new LinkedList<Double>();

    if( depth > 0 )
    {
      // velocity -> use all nodes by iso-parametric interpolation
      for( int i = 0; i < m_midsideNodes.size(); i++ )
      {
        final GMLNodeResult node = m_midsideNodes.get( i );

        sumVxMid = sumVxMid + node.getVelocity().get( 0 );
        sumVyMid = sumVyMid + node.getVelocity().get( 1 );
      }

      /*
       * iso-parametric interpolation of the velocity at the center of the element (isoparam.coord.=(0, 0))
       * (isoparam.coord.=(1./3., 1./3.) for triangle and (0, 0) for quad)
       */

      // TODO: check this by literature. That was directly taken from the BCE2D-post-processing!!
      double w1 = 0;
      double w2 = 0;

      if( m_cornerNodes.size() == 3 )
      {
        w1 = -1.0 / 9.0;
        w2 = 4.0 / 9.0;
      }
      else if( m_cornerNodes.size() == 4 )
      {
        w1 = -1.0 / 4.0;
        w2 = 1.0 / 2.0;
      }

      double vx = sumVxMid * w2 + sumVxCorner * w1;
      double vy = sumVyMid * w2 + sumVyCorner * w1;

      velocity.add( vx );
      velocity.add( vy );
    }
    else
    {
      depth = 0;
      velocity.add( 0.0 );
      velocity.add( 0.0 );
    }
    m_centerNode.setDepth( depth );
    m_centerNode.setVelocity( velocity );
  }

  /**
   * assigns the water level to a dry node from the nearest wetted node connected to the current node by an arc.
   */
  private void assignWaterlevel( GMLNodeResult node, GMLNodeResult minDistNode )
  {
    final double waterlevel = minDistNode.getWaterlevel();
    final double depth = waterlevel - node.getPoint().getZ();
    if( depth > 0 )
      node.setResultValues( 0, 0, depth, waterlevel );
    else
      node.setResultValues( 0, 0, 0, waterlevel );
  }

  /**
   * checks for the dry nodes of an element if there is a wet node as neighbor. if this is the case the waterlevel of
   * the found wet node gets assigned to the dry node. This is important in order to get the proper inundation line.
   */
  public void checkWaterlevels( )
  {
    for( int i = 0; i < m_cornerNodes.size(); i++ )
    {
      final GMLNodeResult node = m_cornerNodes.get( i );

      if( node.getDepth() <= 0 && node.isAssigned() == false )
      {
        /* node is dry */
        // search for wet neighboring nodes and add them to a list
        List<GMLNodeResult> neighborNodeList = new LinkedList<GMLNodeResult>();

        // get the arcs of the dry node
        final List<ArcResult> arcs = node.getArcs();

        // get the wet nodes of that arcs
        neighborNodeList = getNeighbors( node, arcs );

        if( neighborNodeList.size() != 0 ) // the complete element is dry (nothing to do)
        {
          // get the nearest wet node
          double minDistance = Double.MAX_VALUE;
          double distance;
          GMLNodeResult minDistNode = null;

          for( GMLNodeResult currentNode : neighborNodeList )
          {
            if( isPartOfElement( currentNode ) == true )
            {

              distance = currentNode.getPoint().distance( node.getPoint() );
              if( distance < minDistance )
              {
                minDistance = distance;
                minDistNode = currentNode;
              }
            }
          }
          if( minDistNode != null )
          {
            assignWaterlevel( node, minDistNode );
            node.setAssigned( true );
            // if a node gets an assigned water level and becomes wet, maybe that has influence to other dry nodes of
            // the element which where processed before the current node -> therefore another water level check.
            if( node.isWet() == true )
              checkWaterlevels();
          }
        }
      }
      else
        isWet = true;
    }
  }

  /**
   * checks if a given node is part of the corner nodes of the element.
   * 
   * @param currentNode
   *            the node to be checked
   * 
   */
  private boolean isPartOfElement( GMLNodeResult currentNode )
  {
    for( INodeResult node : m_cornerNodes )
    {
      if( node.equals( currentNode ) )
        return true;
    }
    return false;
  }

  private List<GMLNodeResult> getNeighbors( final GMLNodeResult node, final List<ArcResult> arcs )
  {
    final List<GMLNodeResult> neighborList = new LinkedList<GMLNodeResult>();

    for( ArcResult currentArc : arcs )
    {
      GMLNodeResult neighboringNode;

      if( currentArc.node1ID == node.getNodeID() )
        neighboringNode = currentArc.getNodeUp();
      else
        neighboringNode = currentArc.getNodeDown();

      if( neighboringNode.getDepth() > 0 ) // wet
        neighborList.add( neighboringNode );

    }
    return neighborList;
  }

  public SimpleNodeResult getCenterNode( )
  {
    return m_centerNode;
  }

}
