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

import java.awt.Graphics2D;
import java.awt.image.BufferedImage;

import org.eclipse.swt.graphics.GC;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.model.feature.Feature;

/**
 * A Mark takes a "shape" and applies coloring to it. The shape can be derived either from a well-known name (such as
 * "square"), an external URL in various formats (such as, perhaps GIF), or from a glyph of a font. Multiple external
 * formats may be used with the semantic that they all contain the equivalent shape in different formats. If an image
 * format is used that has inherent coloring, the coloring is discarded and only the opacity channel (or equivalent) is
 * used. A Halo, Fill, and/or Stroke is applied as appropriate for the shape's source format.
 * <p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public interface Mark
{

  /**
   * Gives the well known name of a Mark's shape. Allowed values include at least "square", "circle", "triangle",
   * "star", "cross", and "x", though map servers may draw a different symbol instead if they don't have a shape for all
   * of these. Renderings of these marks may be made solid or hollow depending on Fill and Stroke parameters. The
   * default value is "square".
   * 
   * @return the WK-Name of the mark
   */
  String getWellKnownName( );

  /**
   * Sets the well known name of a Mark's shape. Allowed values include at least "square", "circle", "triangle", "star",
   * "cross", and "x", though map servers may draw a different symbol instead if they don't have a shape for all of
   * these. Renderings of these marks may be made solid or hollow depending on Fill and Stroke parameters. The default
   * value is "square"..
   * 
   * @param wellKnownName
   *            the WK-Name of the mark
   */
  void setWellKnownName( String wellKnownName );

  /**
   * A Fill allows area geometries to be filled. There are two types of fills: solid-color and repeated GraphicFill. In
   * general, if a Fill element is omitted in its containing element, no fill will be rendered. The default is a solid
   * 50%-gray (color "#808080") opaque fill.
   * 
   * @return the fill of the mark
   */
  Fill getFill( );

  /**
   * Sets the Fill
   * 
   * @param fill
   *            the fill of the mark
   */
  void setFill( Fill fill );

  /**
   * A Stroke allows a string of line segments (or any linear geometry) to be rendered. There are three basic types of
   * strokes: solid Color, GraphicFill (stipple), and repeated GraphicStroke. A repeated graphic is plotted linearly and
   * has its graphic symbol bended around the curves of the line string. The default is a solid black line (Color
   * "#000000").
   * 
   * @return the stroke of the mark
   */
  Stroke getStroke( );

  /**
   * Sets the <Stroke>
   * 
   * @param stroke
   *            the stroke of the mark
   */
  void setStroke( Stroke stroke );

  BufferedImage getAsImage( Feature feature, int intSize ) throws FilterEvaluationException;

  void paintAwt( final Graphics2D g, final Feature feature, final int size ) throws FilterEvaluationException;

  void paint( final GC gc, final Feature feature ) throws FilterEvaluationException;

}