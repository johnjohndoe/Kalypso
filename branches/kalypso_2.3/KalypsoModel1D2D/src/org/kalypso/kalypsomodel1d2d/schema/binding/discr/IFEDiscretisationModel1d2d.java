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

import javax.xml.namespace.QName;

import org.kalypso.afgui.model.IModel;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsosimulationmodel.core.discr.IFENetItem;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * Interface for classes representing a feature of the type wb1d2d:FEDiscretisationModel1d2d
 * 
 * @author Patrice Congo
 */
public interface IFEDiscretisationModel1d2d extends IModel
{

  public final static QName QNAME = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "DiscretisationModel" ); //$NON-NLS-1$

  /**
   * QName for a property linking a feature to a node. Use for example in DiscretisationModel feature
   */
  public final static QName WB1D2D_PROP_COMPLEX_ELEMENTS = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "complexElement" ); //$NON-NLS-1$

  /**
   * QName for a property linking a feature to a node. Use for example in DiscretisationModel feature
   */
  public final static QName WB1D2D_PROP_NODES = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "node" ); //$NON-NLS-1$

  /**
   * QName for a property linking a feature to an edge. Use for example in DiscretisationModel feature
   */
  public final static QName WB1D2D_PROP_EDGES = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "edge" ); //$NON-NLS-1$

  public final static QName WB1D2D_PROP_CONTINUITY_LINES = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "continuityLine" ); //$NON-NLS-1$

  public final static QName WB1D2D_PROP_ELEMENTS = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "element" ); //$NON-NLS-1$

  public final static QName WB1D2D_PROP_MESH = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "mesh" ); //$NON-NLS-1$

  public final static QName WB1D2D_PROP_MESH_SURFACE = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "polyhedralSurfaceMember" ); //$NON-NLS-1$

  /**
   * Finds an edge given two bounding nodes. If a the found edge does not have the direction from node0 to node1 a
   * {@link IEdgeInv} is created and return
   * 
   * @param node0
   *          the alledged first node of the edge
   * @param node1
   *          the alleged second node of the edge
   * @return an edge bounded by the given node. An {@link IEdgeInv} in case that a edge starting from node1 to node0
   *         exists
   */
  public IFE1D2DEdge findEdge( final IFE1D2DNode node0, final IFE1D2DNode node1 );

  /**
   * To get the complex element that this discretisation model contains
   * 
   * @return the complex elements this discretisation model contains as {@link IFeatureWrapperCollection}
   */
  public IFeatureWrapperCollection<IFE1D2DComplexElement> getComplexElements( );

  /**
   * Gets the element this discretisation model contains
   * 
   * @return the elements of this discretisation model as {@link IFeatureWrapperCollection}
   */
  public IFeatureWrapperCollection<IFE1D2DElement> getElements( );

  /**
   * To get the edges this feature wrapper contains s
   * 
   * @return the edges that this discetisation model contains as {@link IFeatureWrapperCollection}
   */
  public IFeatureWrapperCollection<IFE1D2DEdge> getEdges( );

  /**
   * gets the nodes this discretisation model contains.
   * 
   * @return the nodes that this discretisation model contains as {@link IFeatureWrapperCollection}
   */
  public IFeatureWrapperCollection<IFE1D2DNode> getNodes( );

  public IFeatureWrapperCollection<IFELine> getContinuityLines( );

  /**
   * Finds the node nearest to the given position, within the search rectangle
   * 
   * @param nodeLocation
   *          the reference location (center of the search rectangle)
   * @param searchRectWidth
   *          the width/height of the search rectangle
   * 
   * @return node nearest to the nodeLocation, or <code>null</code> if no node is found within the search rectangle
   */
  public IFE1D2DNode< ? > findNode( GM_Point nodeLocation, double searchRectWidth );

  /**
   * Creates a node at the specifies position. The is realy created only if there is no node within the a square which
   * center is given by nodeLocation and which width is given by searchRectWidth Search is not done if searchSquareWidth
   * is negativ.
   * 
   * @param nodeLocation
   *          the location for the new node
   * @param searchSquareWidth
   *          the width of the search re
   * @param alreadyExists
   *          if not null and not empty a boolean is set at position 0 which indicates with true that a node already
   *          exists and fals otherwise
   * @return the created or found node
   * 
   */
  public IFE1D2DNode< ? > createNode( GM_Point nodeLocation, double searchSquareWidth, boolean[] alreadyExists );

  public IFELine findContinuityLine( final GM_Point position, double grabDistance );

  public IPolyElement find2DElement( final GM_Point position, final double grabDistance );

  public IElement1D find1DElement( final GM_Point position, final double grabDistance );

  /**
   * Finds the nearest element to the given position
   * 
   * @param position
   *          the search position
   * @param grabDistance
   *          the maximal grab distance
   * @param elementType
   *          the element type
   */
  public <T extends IFENetItem> T findElement( final GM_Point position, final double grabDistance, Class<T> elementType );

  /**
   * Finds the nearest edge to the given position
   * 
   * @param position
   *          the search position
   * @param grabDistance
   *          the maximal grab distance
   */
  public IFE1D2DEdge findEdge( final GM_Point position, final double grabDistance );

}