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

package org.deegree.graphics.sld;

import org.deegree.filterencoding.FilterEvaluationException;
import org.deegree.model.feature.Feature;

/**
 * Incarnation of a sld:Halo-element. A Halo is a type of Fill that is applied
 * to the backgrounds of font glyphs. The use of halos greatly improves the
 * readability of text labels.
 * <p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public interface Halo
{

  /**
   * A Fill allows area geometries to be filled. There are two types of fills:
   * solid-color and repeated GraphicFill. In general, if a Fill element is
   * omitted in its containing element, no fill will be rendered. The default is
   * a solid 50%-gray (color "#808080") opaque fill.
   * <p>
   * 
   * @return the underlying <tt>Fill</tt> -object, or null
   */
  Fill getFill();

  /**
   * Sets the Fill for the Halo.
   * 
   * @param fill
   *          defines the fill color and pattern
   */
  void setFill( Fill fill );

  /**
   * Returns the underlying <tt>Stroke</tt> -instance used to draw the
   * boundary of the halo.
   * <p>
   * 
   * @return the underlying <tt>Stroke</tt> -object, or null
   */
  Stroke getStroke();

  /**
   * Sets the Stroke for the Halo.
   * 
   * @param stroke
   *          defines the stroke color and pattern
   */
  void setStroke( Stroke stroke );

  /**
   * The Radius element gives the absolute size of a halo radius in pixels
   * encoded as a floating-point number. The radius is taken from the outside
   * edge of a font glyph to extend the area of coverage of the glyph (and the
   * inside edge of holes in the glyphs). The halo of a text label is considered
   * to be a single shape. The default radius is one pixel. Negative values are
   * not allowed.
   * <p>
   * 
   * @return the radius definition as <tt>ParameterValueType</tt>, or null if
   *         it has not been specified
   */
  ParameterValueType getRadius();

  /**
   * Sets the Radius for the Halo.
   * 
   * @param radius
   *          radius to be used for the halo, use null for a rectangle styled
   *          halo
   */
  void setRadius( ParameterValueType radius );

  /**
   * The Radius element gives the absolute size of a halo radius in pixels
   * encoded as a floating-point number. The radius is taken from the outside
   * edge of a font glyph to extend the area of coverage of the glyph (and the
   * inside edge of holes in the glyphs). The halo of a text label is considered
   * to be a single shape. The default radius is one pixel. Negative values are
   * not allowed.
   * <p>
   * 
   * @param feature
   *          specifies the <tt>Feature</tt> to be used for evaluation of the
   *          underlying 'sld:ParameterValueType'
   * @return the radius value, or -1 if it has not been specified
   * @throws FilterEvaluationException
   *           if the evaluation fails
   */
  double getRadius( Feature feature ) throws FilterEvaluationException;

  /**
   * @see #getRadius(Feature)
   *      <p>
   * @param radius
   *          radius to be set for the halo
   */
  void setRadius( double radius );
}