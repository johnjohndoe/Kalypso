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
package org.kalypsodeegree.graphics.sld;

/**
 * Used to render an interior "fill" and an outlining "stroke" for a polygon or
 * other 2D-area geometry. If a point or line are used, the fill is ignored and
 * the stroke is used as described in the LineSymbol. A missing Geometry element
 * selects the default geometry. A missing Fill or Stroke element means that
 * there will be no fill or stroke plotted, respectively. The contained elements
 * are in the conceptual order of their being used and plotted using the
 * "painters model", where the Fill will be rendered first, and then the Stroke
 * will be rendered on top of the Fill.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public interface PolygonSymbolizer extends Symbolizer
{

  /**
   * A Fill allows area geometries to be filled. There are two types of fills:
   * solid-color and repeated GraphicFill. In general, if a Fill element is
   * omitted in its containing element, no fill will be rendered. The default is
   * a solid 50%-gray (color "#808080") opaque fill.
   * 
   * @return the fill of the polygon
   */
  Fill getFill();

  /**
   * Sets the Fill
   * 
   * @param fill
   *          the fill of the polygon
   */
  void setFill( Fill fill );

  /**
   * A Stroke allows a string of line segments (or any linear geometry) to be
   * rendered. There are three basic types of strokes: solid Color, GraphicFill
   * (stipple), and repeated GraphicStroke. A repeated graphic is plotted
   * linearly and has its graphic symbol bended around the curves of the line
   * string. The default is a solid black line (Color "#000000").
   * 
   * @return the stroke of the polygon
   */
  Stroke getStroke();

  /**
   * Sets the Stroke
   * 
   * @param stroke
   *          the stroke of the polygon
   */
  void setStroke( Stroke stroke );
}