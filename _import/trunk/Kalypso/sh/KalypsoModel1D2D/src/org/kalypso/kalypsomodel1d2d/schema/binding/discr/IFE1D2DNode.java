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
package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import java.util.Collection;

import org.kalypso.kalypsosimulationmodel.core.discr.IFENode;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * Interface for classes representing an finite element node
 * of the 1D, 2D model.
 * 
 * @author Patrice Congo
 *
 */
public interface IFE1D2DNode<CT extends IFE1D2DEdge> extends IFENode/*<CT>*/
{
  /**
   * To get the position of this fe-node
   * @return the position of this node as {@link GM_Point}
   */
  public GM_Point getPoint( );
  
  /**
   * To set the position of this node to the given point
   * @param the new position to set as {@link GM_Point} 
   *        note that null is a legal value
   * 
   * 
   */
  public void setPoint(GM_Point newLocation);

  /**
   * To get all elements containing this node
   * @return the element containing this  as array
   */
  public IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge>[] getElements( );
  
//  /**
//   * To get all edges containing this node
//   * @return all edges containing this node as array
//   */
//  public IFE1D2DEdge<IFE1D2DElement, IFE1D2DNode>[] getEdges( );

  /**
   * Gets all neighbours of this node. Neighbours nodes are nodes
   * that belongs to an element containing this node
   */
  public Collection<IFE1D2DNode> getNeighbours( );
  
  /**
   * Add a container (typically an Edge) to the node
   * @param the id of the container
   * 
   */
  public void addContainer(String linkRef);
  
  /**
   * Get the containers of this node, typically edges
   * @return the containers of this node as {@link IFeatureWrapperCollection}
   */
  public  IFeatureWrapperCollection<CT> getContainers();
  
}
