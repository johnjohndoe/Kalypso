/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsosimulationmodel.core.discr.IFENetItem;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * Interface for the class that represents a finite element edge. An edge typically have 2 nodes and (possibly) inverted
 * edge.
 *
 * @author Patrice Congo
 * @author Dejan Antanaskovic
 */
public interface IFE1D2DEdge extends IFENetItem
{
  QName QNAME = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "Edge" ); //$NON-NLS-1$

  QName WB1D2D_PROP_EDGE_CONTAINERS = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "edgeContainer" ); //$NON-NLS-1$

  QName WB1D2D_PROP_MIDDLE_NODE = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "middleNode" ); //$NON-NLS-1$

  QName WB1D2D_PROP_DIRECTEDNODE = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "directedNode" ); //$NON-NLS-1$

  QName WB1D2D_PROP_MIDDLE_GEOM = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "geometry" ); //$NON-NLS-1$

  /**
   * Adds the a container to this edge. edge container are typically finite elements.
   *
   * @param containerID
   *          the id of the container
   */
  void addContainer( String containerID );

  /**
   * To get the edge node at the given index
   *
   * @param index
   *          the index of the node; valid values are 0, 1
   *
   * @throws IndexOutOfBoundsException
   *           if the index is
   *           <ul>
   *           <li/>less than 0 or greater than 1
   *           <li/>but also if the edge contains no element or the edge contains 1 node and the index is 1
   *           <ul>
   * @see #getNodes()
   */
  IFE1D2DNode getNode( int index ) throws IndexOutOfBoundsException;

  /**
   * Add a node to this edge.
   *
   * @param the
   *          id of the node
   * @throws IllegalArgumentException
   *           if nodeID is null
   * @throws ArrayIndexOutOfBoundsException
   *           if there are already 2 node in this edge
   *
   */
  void addNode( String nodeID );

  /**
   * Returns the middle node of this edge, or <code>null</code> if it is not defined.
   *
   * @return the middle node of the edge
   */
  IFE1D2DNode getMiddleNode( );

  /**
   * Returns the middle node of this edge.
   *
   * @param createIfNotExists
   *          if true, middle node will be created if not exists (same as {@link #getMiddleNode()})<br>
   *          if false, middle node will be returned if exists, <code>null</code> otherwise
   *
   * @see #getMiddleNode()
   * @return the middle node of the edge
   */
  GM_Point getMiddleNodePoint( );

  /**
   * Sets the middle node of this edge
   *
   * @param middleNode
   *          the new middle node of this edge. null is a legal value.
   *
   */
  void setMiddleNode( IFE1D2DNode middleNode );

  /**
   * To get the containers(typically elements) of this edge
   *
   * @return the containers of this edge as {@link IFeatureBindingCollection}
   */
  IFeatureBindingCollection<IFE1D2DElement> getContainers( );

  /**
   * To get the nodes of this edge
   *
   * @param to
   *          get the node of this edge as {@link IFeatureBindingCollection}
   * @see #getNode(int)
   */
  IFeatureBindingCollection<IFE1D2DNode> getNodes( );

  /**
   * An edge is inside the net if:
   * <ul>
   * <li>it has exactly to element, or</li>
   * <li>it has exactly one element and an associated inverted edge</li>
   * </ul>
   *
   * @return <code>true</code>, if this edge lies on the border of the net.
   */
  boolean isBorder( );

  IFeatureBindingCollection<IFE1D2DElement> getAdjacentElements( );

  GM_Curve getGeometry( );
}
