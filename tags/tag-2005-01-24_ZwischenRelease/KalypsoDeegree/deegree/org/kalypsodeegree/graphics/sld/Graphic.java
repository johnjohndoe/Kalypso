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

import java.awt.image.BufferedImage;

import org.deegree.filterencoding.FilterEvaluationException;
import org.deegree.model.feature.Feature;

/**
 * A Graphic is a "graphic symbol" with an inherent shape, color, and size.
 * Graphics can either be referenced from an external URL in a common format
 * (such as GIF or SVG) or may be derived from a Mark. Multiple external URLs
 * may be referenced with the semantic that they all provide the same graphic in
 * different formats. The "hot spot" to use for rendering at a point or the
 * start and finish handle points to use for rendering a graphic along a line
 * must either be inherent in the external format or are system- dependent. The
 * default size of an image format (such as GIF) is the inherent size of the
 * image. The default size of a format without an inherent size is 16 pixels in
 * height and the corresponding aspect in width. If a size is specified, the
 * height of the graphic will be scaled to that size and the corresponding
 * aspect will be used for the width. The default if neither an ExternalURL nor
 * a Mark is specified is to use the default Mark with a size of 6 pixels. The
 * size is in pixels and the rotation is in degrees clockwise, with 0 (default)
 * meaning no rotation. In the case that a Graphic is derived from a font-glyph
 * Mark, the Size specified here will be used for the final rendering. Allowed
 * CssParameters are "opacity", "size", and "rotation".
 * <p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public interface Graphic
{

  // default values
  public static final double OPACITY_DEFAULT = 1.0;

  public static final double SIZE_DEFAULT = -1;

  public static final double ROTATION_DEFAULT = 0.0;

  /**
   * Returns a <tt>BufferedImage</tt> representing this object. The image
   * respects the 'Opacity', 'Size' and 'Rotation' parameters.
   * <p>
   * 
   * @return the <tt>BufferedImage</tt> ready to be painted
   * @throws FilterEvaluationException
   *           if the evaluation fails
   */
  BufferedImage getAsImage( Feature feature ) throws FilterEvaluationException;

  /**
   * Sets a <tt>BufferedImage</tt> representing this object. The image
   * respects the 'Opacity', 'Size' and 'Rotation' parameters.
   * <p>
   * 
   * @param bufferedImage
   *          BufferedImage to be set
   */
  void setAsImage( BufferedImage bufferedImage );

  /**
   * Returns an object-array that enables the access to the stored
   * <tt>ExternalGraphic</tt> and <tt>Mark</tt> -instances.
   * <p>
   * 
   * @return contains <tt>ExternalGraphic</tt> and <tt>Mark</tt> -objects
   */
  public Object[] getMarksAndExtGraphics();

  /**
   * Sets the <tt>ExternalGraphic</tt>/<tt>Mark<tt>-instances that the image
   * will be based on.
   * <p>
   * @param object to be used as basis for the resulting image
   */
  public void setMarksAndExtGraphics( Object[] object );

  /**
   * Adds an Object to an object-array that enables the access to the stored
   * <tt>ExternalGraphic</tt> and <tt>Mark</tt> -instances.
   * <p>
   * 
   * @param object
   */
  public void addMarksAndExtGraphic( Object object );

  /**
   * Removes an Object from an object-array that enables the access to the
   * stored <tt>ExternalGraphic</tt> and <tt>Mark</tt> -instances.
   * <p>
   * 
   * @param object
   */
  public void removeMarksAndExtGraphic( Object object );

  /**
   * The Opacity element gives the opacity of to use for rendering the graphic.
   * <p>
   * 
   * @param feature
   *          specifies the <tt>Feature</tt> to be used for evaluation of the
   *          underlying 'sld:ParameterValueType'
   * @return the (evaluated) value of the parameter
   * @throws FilterEvaluationException
   *           if the evaluation fails or the value is invalid
   */
  double getOpacity( Feature feature ) throws FilterEvaluationException;

  /**
   * The Opacity element gives the opacity of to use for rendering the graphic.
   * <p>
   * 
   * @param opacity
   *          Opacity to be set for the graphic
   */
  void setOpacity( double opacity );

  /**
   * The Size element gives the absolute size of the graphic in pixels encoded
   * as a floating-point number. This element is also used in other contexts
   * than graphic size and pixel units are still used even for font size. The
   * default size for an object is context-dependent. Negative values are not
   * allowed.
   * <p>
   * 
   * @param feature
   *          specifies the <tt>Feature</tt> to be used for evaluation of the
   *          underlying 'sld:ParameterValueType'
   * @return the (evaluated) value of the parameter
   * @throws FilterEvaluationException
   *           if the evaluation fails or the value is invalid
   */
  double getSize( Feature feature ) throws FilterEvaluationException;

  /**
   * @see #getSize(Feature)
   *      <p>
   * @param size
   *          size to be set for the graphic
   */
  void setSize( double size );

  /**
   * The Rotation element gives the rotation of a graphic in the clockwise
   * direction about its center point in radian, encoded as a floating- point
   * number. Negative values mean counter-clockwise rotation. The default value
   * is 0.0 (no rotation).
   * <p>
   * 
   * @param feature
   *          specifies the <tt>Feature</tt> to be used for evaluation of the
   *          underlying 'sld:ParameterValueType'
   * @return the (evaluated) value of the parameter
   * @throws FilterEvaluationException
   *           if the evaluation fails or the value is invalid
   */
  double getRotation( Feature feature ) throws FilterEvaluationException;

  /**
   * @see #getRotation(Feature)
   *      <p>
   * @param rotation
   *          rotation to be set for the graphic
   */
  void setRotation( double rotation );
}