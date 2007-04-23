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

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEMiddleNode;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;


/**
 * Provide some operation on edges
 *  
 * @author Patrice Congo
 *
 */
public class EdgeOps
{
  private EdgeOps()
  {
    
  }
  
  public static final GM_Point computeEdgeMiddle(IFE1D2DEdge edge)
  {
    Assert.throwIAEOnNullParam( edge, "edge" );
    IFEMiddleNode middleNode = edge.getMiddleNode();
    
    if(middleNode!=null)
    {
      //INFO This have concequences on manipulation the return 
      //point afterwards
      return middleNode.getPoint();
    }
    
    IFE1D2DNode node0 = edge.getNode( 0 );
    IFE1D2DNode node1 = edge.getNode( 1 );
    if(node0==null || node1==null)
    {
      throw new IllegalArgumentException(
          "Edge does not has too nodes:"+
          "node0"+node0+
          "node1"+node1); 
    }
    GM_Point point0 = node0.getPoint();
    GM_Point point1 = node1.getPoint();
    if(point0==null)
    {
      throw new IllegalArgumentException(
          "Node0 does not have a location ser:"+
          "node0"+node0); 
    }
    if(point1==null)
    {
      throw new IllegalArgumentException(
          "Node1 does not have a location ser:"+
          "node1"+node1); 
    }
    final int dim= Math.min( 
                point0.getCoordinateDimension(),
                point1.getCoordinateDimension());
    if(dim==2)
    {
      double middleX=(point0.getX()+point1.getX())/2;
      double middleY=(point0.getY()+point1.getY())/2;
      return GeometryFactory.createGM_Point( 
                middleX, middleY, point0.getCoordinateSystem() );
    }
    else if(dim==3)
    {
      double middleX=(point0.getX()+point1.getX())/2;
      double middleY=(point0.getY()+point1.getY())/2;
      double middleZ=(point0.getZ()+point1.getZ())/2;
      return GeometryFactory.createGM_Point( 
                      middleX, middleY,middleZ, 
                      point0.getCoordinateSystem() );
    }
    else
    {
      throw new IllegalArgumentException(
          "Edge node point dimention must be 2 or 3:"+
          "\n\tcurrent dimension="+dim);
    }
  }
  
  /**
   * To get a border node of the 1d edge.
   * If the edge has 2 border nodes it end node is return.
   * 
   * @param elementEdge the edge which  border node is to be retrieved
   * 
   * @return the border node of the given 1d node
   *  
   */
  public static final IFE1D2DNode<IFE1D2DEdge> find1DEdgeEndNode(IFE1D2DEdge elementEdge )
  {
    if(!TypeInfo.is1DEdge( elementEdge ))
    {
      throw new IllegalArgumentException(
          "Must be 1D node but is:"+
          "\n\tElementQName="+elementEdge.getWrappedFeature().getFeatureType().getQName()+
          "\n\tValue="+elementEdge);
    }
    IFE1D2DNode node1 = elementEdge.getNode( 1 );
    if(node1.getContainers().size()==1)
    {
      return node1;
    }
    IFE1D2DNode node0 = elementEdge.getNode( 0 );
    if(node0.getContainers().size()==1)
    {
      return node0;
    }
    return null;
  }
  
  /**
   * Checks whether the given edge is an isolated edge.
   * An isolated edge is an edge without any border edge. 
   * 
   * @param elementEdge the edge to check for isolation
   * 
   * @return true is the provided edge is an isolated edge 
   *            false otherwise
   *  
   */
  public static final boolean isIsolatedEdge(IFE1D2DEdge edge )
  {
    Assert.throwIAEOnNullParam( edge, "edge" );
    
    IFE1D2DNode node1 = edge.getNode( 1 );
    if(node1.getContainers().size()!=1)
    {
      return false;
    }
    
    IFE1D2DNode node0 = edge.getNode( 0 );
    if(node0.getContainers().size()!=1)
    {
      return false;
    }
    
    return true;
  }
}
