/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

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

 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de

 ---------------------------------------------------------------------------*/
package org.deegree.graphics.sld;

import java.awt.Color;

import org.deegree.model.feature.Feature;
import org.deegree.services.wfs.filterencoding.FilterEvaluationException;

/**
 * A Stroke allows a string of line segments (or any linear geometry) to be
 * rendered. There are three basic types of strokes: solid Color, GraphicFill
 * (stipple), and repeated GraphicStroke. A repeated graphic is plotted linearly
 * and has its graphic symbol bended around the curves of the line string. The
 * default is a solid black line (Color "#000000").
 * <p>
 * The supported CSS-Parameter names are:
 * <ul>
 * <li>stroke (color)
 * <li>stroke-opacity
 * <li>stroke-width
 * <li>stroke-linejoin
 * <li>stroke-linecap
 * <li>stroke-dasharray
 * <li>stroke-dashoffset
 * <p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public interface Stroke extends Drawing
{

  public static final int LJ_MITRE = java.awt.BasicStroke.JOIN_MITER;

  public static final int LJ_ROUND = java.awt.BasicStroke.JOIN_ROUND;

  public static final int LJ_BEVEL = java.awt.BasicStroke.JOIN_BEVEL;

  public static final int LC_BUTT = java.awt.BasicStroke.CAP_BUTT;

  public static final int LC_ROUND = java.awt.BasicStroke.CAP_ROUND;

  public static final int LC_SQUARE = java.awt.BasicStroke.CAP_SQUARE;

  // default values
  public static final Color COLOR_DEFAULT = Color.decode( "#000000" );

  public static final double OPACITY_DEFAULT = 1.0;

  public static final double WIDTH_DEFAULT = 1.0;

  public static final int LJ_DEFAULT = LJ_MITRE;

  public static final int LC_DEFAULT = LC_BUTT;

  /**
   * The GraphicStroke element both indicates that a repeated-linear-graphic
   * stroke type will be used.
   * <p>
   * 
   * @returns the underlying <tt>GraphicStroke</tt> instance (may be null)
   */
  GraphicStroke getGraphicStroke();

  /**
   * Sets a GraphicStroke.
   * 
   * @param graphicStroke
   *          the graphicStroke element
   */
  void setGraphicStroke( GraphicStroke graphicStroke );

  /**
   * The stroke CssParameter element gives the solid color that will be used for
   * a stroke. The color value is RGB-encoded using two hexadecimal digits per
   * primary-color component, in the order Red, Green, Blue, prefixed with a
   * hash (#) sign. The hexadecimal digits between A and F may be in either
   * uppercase or lowercase. For example, full red is encoded as #ff0000 (with
   * no quotation marks). The default color is defined to be black (#000000) in
   * the context of the LineSymbolizer, if the stroke CssParameter element is
   * absent.
   * <p>
   * 
   * @param feature
   *          specifies the <tt>Feature</tt> to be used for evaluation of the
   *          underlying 'sld:ParameterValueType'
   * @return the (evaluated) value of the parameter
   * @throws FilterEvaluationException
   *           if the evaluation fails
   */
  Color getStroke( Feature feature ) throws FilterEvaluationException;

  /**
   * @see getStroke
   *      <p>
   * @param stroke
   *          the stroke to be set
   */
  void setStroke( Color stroke );

  /**
   * The stroke-opacity CssParameter element specifies the level of translucency
   * to use when rendering the stroke. The value is encoded as a floating-point
   * value (float) between 0.0 and 1.0 with 0.0 representing completely
   * transparent and 1.0 representing completely opaque, with a linear scale of
   * translucency for intermediate values. For example, 0.65 would represent 65%
   * opacity. The default value is 1.0 (opaque).
   * <p>
   * 
   * @param feature
   *          specifies the <tt>Feature</tt> to be used for evaluation of the
   *          underlying 'sld:ParameterValueType'
   * @return the (evaluated) value of the parameter
   * @throws FilterEvaluationException
   *           if the evaluation fails
   */
  double getOpacity( Feature feature ) throws FilterEvaluationException;

  /**
   * @see getOpacity
   *      <p>
   * @param opacity
   *          the opacity to be set for the stroke
   */
  void setOpacity( double opacity );

  /**
   * The stroke-width CssParameter element gives the absolute width (thickness)
   * of a stroke in pixels encoded as a float. (Arguably, more units could be
   * provided for encoding sizes, such as millimeters or typesetter's points.)
   * The default is 1.0. Fractional numbers are allowed (with a system-dependent
   * interpretation) but negative numbers are not.
   * <p>
   * 
   * @param feature
   *          specifies the <tt>Feature</tt> to be used for evaluation of the
   *          underlying 'sld:ParameterValueType'
   * @return the (evaluated) value of the parameter
   * @throws FilterEvaluationException
   *           if the evaluation fails
   */
  double getWidth( Feature feature ) throws FilterEvaluationException;

  /**
   * @see getWidth
   *      <p>
   * @param width
   *          the width to be set for the stroke
   */
  void setWidth( double width );

  /**
   * The stroke-linejoin CssParameter element encode enumerated values telling
   * how line strings should be joined (between line segments). The values are
   * represented as content strings. The allowed values for line join are mitre,
   * round, and bevel.
   * <p>
   * 
   * @param feature
   *          specifies the <tt>Feature</tt> to be used for evaluation of the
   *          underlying 'sld:ParameterValueType'
   * @return the (evaluated) value of the parameter
   * @throws FilterEvaluationException
   *           if the evaluation fails
   */
  int getLineJoin( Feature feature ) throws FilterEvaluationException;

  /**
   * @see getLineJoin
   *      <p>
   * @param lineJoin
   *          the lineJoin to be set for the stroke
   */
  void setLineJoin( int lineJoin );

  /**
   * Thestroke-linecap CssParameter element encode enumerated values telling how
   * line strings should be capped (at the two ends of the line string). The
   * values are represented as content strings. The allowed values for line cap
   * are butt, round, and square. The default values are system-dependent.
   * <p>
   * 
   * @param feature
   *          specifies the <tt>Feature</tt> to be used for evaluation of the
   *          underlying 'sld:ParameterValueType'
   * @return the (evaluated) value of the parameter
   * @throws FilterEvaluationException
   *           if the evaluation fails
   */
  int getLineCap( Feature feature ) throws FilterEvaluationException;

  /**
   * @see getLineCap
   *      <p>
   * @param lineCap
   *          lineCap to be set for the stroke
   */
  void setLineCap( int lineCap );

  /**
   * The stroke-dasharray CssParameter element encodes a dash pattern as a
   * series of space separated floats. The first number gives the length in
   * pixels of dash to draw, the second gives the amount of space to leave, and
   * this pattern repeats. If an odd number of values is given, then the pattern
   * is expanded by repeating it twice to give an even number of values. Decimal
   * values have a system-dependent interpretation (usually depending on whether
   * antialiasing is being used). The default is to draw an unbroken line.
   * <p>
   * 
   * @param feature
   *          specifies the <tt>Feature</tt> to be used for evaluation of the
   *          underlying 'sld:ParameterValueType'
   * @return the (evaluated) value of the parameter (null if the parameter was
   *         not specified)
   * @throws FilterEvaluationException
   *           if the evaluation fails
   */
  float[] getDashArray( Feature feature ) throws FilterEvaluationException;

  /**
   * @see getDashArray
   *      <p>
   * @param dashArray
   *          the dashArray to be set for the Stroke
   */
  void setDashArray( float[] dashArray );

  /**
   * The stroke-dashoffset CssParameter element specifies the distance as a
   * float into the stroke-dasharray pattern at which to start drawing.
   * <p>
   * 
   * @param feature
   *          specifies the <tt>Feature</tt> to be used for evaluation of the
   *          underlying 'sld:ParameterValueType'
   * @return the (evaluated) value of the parameter
   * @throws FilterEvaluationException
   *           if the evaluation fails
   */
  float getDashOffset( Feature feature ) throws FilterEvaluationException;

  /**
   * The stroke-dashoffset CssParameter element specifies the distance as a
   * float into the stroke-dasharray pattern at which to start drawing.
   * <p>
   * 
   * @param dashOffset
   *          the dashOffset to be set for the Stroke
   */
  void setDashOffset( float dashOffset );
}