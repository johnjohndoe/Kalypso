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

/**
 * Provide some operation on edges
 * 
 * @author Patrice Congo
 */
public class EdgeOps
{
  /**
   * To get a border node of the 1d edge. If the edge has 2 border nodes it end node is return.
   * 
   * @param elementEdge
   *          the edge which border node is to be retrieved
   * @return the border node of the given 1d node
   */
  public static final IFE1D2DNode find1DEdgeEndNode( final IFE1D2DEdge elementEdge )
  {
    final IFE1D2DNode[] nodes = elementEdge.getNodes();
    final IFE1D2DNode node1 = nodes[1];
    if( node1.getLinkedEdges().length == 1 )
    {
      return node1;
    }
    final IFE1D2DNode node0 = nodes[0];
    if( node0.getLinkedEdges().length == 1 )
    {
      return node0;
    }
    return null;
  }

}
