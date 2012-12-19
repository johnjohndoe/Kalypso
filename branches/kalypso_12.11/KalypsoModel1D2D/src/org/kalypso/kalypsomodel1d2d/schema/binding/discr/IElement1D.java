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

import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypsodeegree.model.geometry.GM_Curve;

/**
 * @author Gernot Belger
 */
public interface IElement1D extends IFE1D2DElement
{
  QName QNAME = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "Element1D" ); //$NON-NLS-1$

  QName QNAME_PROP_GEOMETRY = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "geometry" ); //$NON-NLS-1$

  /**
   * Returns the current edge of this 1d-element.
   *
   * @return null, if no edge is currently associated.
   */
  IFE1D2DEdge getEdge( );

  /**
   * Sets (the reference to) the edge of this 1d-element.
   * <p>
   * Also makes sure that:
   * <ul>
   * <li>the back reference from the edge to this element is set/updated</li>
   * <li>the envelope of this element is invalidated</li>
   * </ul>
   */
  void setEdge( IFE1D2DEdge edge );
  
  @Override
  public GM_Curve getGeometry( );
}