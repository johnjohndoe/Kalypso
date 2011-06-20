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

import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsosimulationmodel.core.discr.IFENetItem;
import org.kalypso.preferences.IKalypsoDeegreePreferences;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * Interface for classes representing an finite element node of the 1D, 2D model.
 * 
 * @author Patrice Congo
 * 
 */
public interface IFE1D2DNode<CT extends IFENetItem> extends IFENetItem
{
  public static final QName QNAME = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "Node" ); //$NON-NLS-1$

  public final static QName WB1D2D_PROP_NODE_CONTAINERS = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "nodeContainer" ); //$NON-NLS-1$

  public final static QName WB1D2D_PROP_POINT = new QName( NS.GML3, "pointProperty" ); //$NON-NLS-1$

  public static final QName PROP_HAS_ELEVATION = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "hasElevation" ); //$NON-NLS-1$

  public static final String DEFAULT_COORDINATE_SYSTEM = IKalypsoDeegreePreferences.DEFAULT_CRS_VALUE;

  public static final QName PROP_GEOMETRY = new QName( NS.GML3, "pointProperty" ); //$NON-NLS-1$

  /**
   * To get the position of this fe-node
   * 
   * @return the position of this node as {@link GM_Point}
   */
  public GM_Point getPoint( );

  /**
   * To set the position of this node to the given point
   * 
   * @param the
   *          new position to set as {@link GM_Point} note that null is a legal value
   * 
   * 
   */
  public void setPoint( GM_Point newLocation );

  /**
   * To get all elements containing this node
   * 
   * @return the element containing this as array
   */
  public IFE1D2DElement<IFE1D2DComplexElement, IFE1D2DEdge>[] getElements( );

  /**
   * Gets all neighbours of this node. Neighbours nodes are nodes on the other side of all edges ending with this node.
   */
  public List<IFE1D2DNode> getNeighbours( );

  /**
   * Add a container (typically an Edge) to the node
   * 
   * @param the
   *          id of the container
   * 
   */
  public void addContainer( String linkRef );

  /**
   * Get the containers of this node, typically edges
   * 
   * @return the containers of this node as {@link IFeatureBindingCollection}
   */
  public IFeatureBindingCollection<CT> getContainers( );

}
