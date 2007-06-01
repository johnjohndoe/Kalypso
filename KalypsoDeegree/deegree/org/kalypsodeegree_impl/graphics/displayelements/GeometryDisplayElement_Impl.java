/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 
 history:
 
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
 
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.graphics.displayelements;

import java.io.Serializable;

import org.kalypsodeegree.graphics.displayelements.GeometryDisplayElement;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Object;

/**
 * Basic interface of all display elements that are related to a geometry. Usually this will be the case.
 * <p>
 * ------------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
abstract class GeometryDisplayElement_Impl extends DisplayElement_Impl implements GeometryDisplayElement, Serializable
{
  /** Use serialVersionUID for interoperability. */
  private final static long serialVersionUID = 465725117946501686L;

  private GM_Object m_geometry = null;

  private Symbolizer m_symbolizer = null;

  /**
   * Creates a new GeometryDisplayElement_Impl object.
   * 
   * @param feature
   * @param geometry
   */
  GeometryDisplayElement_Impl( final Feature feature, final GM_Object geometry )
  {
    super( feature );
    setGeometry( geometry );
  }

  /**
   * Creates a new GeometryDisplayElement_Impl object.
   * 
   * @param feature
   * @param geometry
   * @param symbolizer
   */
  GeometryDisplayElement_Impl( final Feature feature, final GM_Object geometry, final Symbolizer symbolizer )
  {
    super( feature );
    setGeometry( geometry );
    setSymbolizer( symbolizer );
  }

  /**
   * Overwrites the default placement of the <tt>DisplayElement</tt>. This method is used by the
   * <tt>PlacementOptimizer</tt> to minimize the overlapping of labels, for example.
   * <p>
   * 
   * @param o
   *            the placement to be used
   */
  public void setPlacement( final Object o )
  {
  }

  /**
   * sets the geometry that determines the position the DisplayElement will be rendered to
   */
  public void setGeometry( final GM_Object geometry )
  {
    this.m_geometry = geometry;
  }

  /**
   * returns the geometry that determines the position the DisplayElement will be rendered to
   */
  public GM_Object getGeometry( )
  {
    return m_geometry;
  }

  /**
   * sets the rules that determines how the geometry will be rendered
   */
  public void setSymbolizer( final Symbolizer symbolizer )
  {
    this.m_symbolizer = symbolizer;
  }

  /**
   * Returns the symbolizer that determines how the geometry will be rendered.
   */
  public Symbolizer getSymbolizer( )
  {
    return m_symbolizer;
  }

  /**
   * Returns if the <tt>DisplayElement</tt> should be painted at the current scale or not.
   */
  @Override
  public boolean doesScaleConstraintApply( final double scale )
  {
    return m_symbolizer.getMinScaleDenominator() <= scale && m_symbolizer.getMaxScaleDenominator() > scale;
  }

}