/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree.graphics.sld;

import java.awt.Color;

import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.model.feature.Feature;

/**
 * The Font element identifies a font of a certain family, style, weight, size and color.
 * <p>
 * The supported CSS-Parameter names are:
 * <ul>
 * <li>font-family (may be null)
 * <li>font-style (may be null)
 * <li>font-weight (may be null)
 * <li>font-size (may be null)
 * <li>font-color (may be null)
 * <p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public interface Font
{

  public static final int STYLE_NORMAL = java.awt.Font.PLAIN;

  public static final int STYLE_ITALIC = java.awt.Font.ITALIC;

  public static final int STYLE_OBLIQUE = java.awt.Font.ITALIC;

  public static final int WEIGHT_NORMAL = java.awt.Font.PLAIN;

  public static final int WEIGHT_BOLD = java.awt.Font.BOLD;

  public static final int SIZE_DEFAULT = 10;

  public static final Color COLOR_DEFAULT = new Color( 127, 127, 127 );

  /**
   * Returns the (evaluated) value of the font's CssParameter 'font-family'.
   * <p>
   * 
   * @param feature
   *          specifies the <tt>Feature</tt> to be used for evaluation of the underlying 'sld:ParameterValueType'
   * @return the (evaluated) <tt>String</tt> value of the parameter
   * @throws FilterEvaluationException
   *           if the evaluation fails
   */
  String getFamily( Feature feature ) throws FilterEvaluationException;

  /**
   * Sets the value of the font's CssParameter 'font-family'.
   * <p>
   * 
   * @param family
   *          font family to be set
   */
  void setFamily( String family );

  /**
   * Returns the (evaluated) value of the font's CssParameter 'font-style'.
   * <p>
   * 
   * @param feature
   *          specifies the <tt>Feature</tt> to be used for evaluation of the underlying 'sld:ParameterValueType'
   * @return the (evaluated) value of the parameter
   * @throws FilterEvaluationException
   *           if the evaluation fails
   */
  int getStyle( Feature feature ) throws FilterEvaluationException;

  /**
   * Sets the value of the font's CssParameter 'font-style'.
   * <p>
   * 
   * @param style
   *          font-style to be set
   */
  void setStyle( int style );

  /**
   * Returns the (evaluated) value of the font's CssParameter 'font-weight' as a <tt>ParameterValueType</tt>.
   * <p>
   * 
   * @param feature
   *          specifies the <tt>Feature</tt> to be used for evaluation of the underlying 'sld:ParameterValueType'
   * @return the (evaluated) value of the parameter
   * @throws FilterEvaluationException
   *           if the evaluation fails
   */
  int getWeight( Feature feature ) throws FilterEvaluationException;

  /**
   * Sets the value of the font's CssParameter 'font-weight'.
   * <p>
   * 
   * @param weight
   *          font-weight to be set
   */
  void setWeight( int weight );

  /**
   * Returns the (evaluated) value of the font's CssParameter 'font-size'.
   * <p>
   * 
   * @param feature
   *          specifies the <tt>Feature</tt> to be used for evaluation of the underlying 'sld:ParameterValueType'
   * @return the (evaluated) value of the parameter
   * @throws FilterEvaluationException
   *           if the evaluation fails
   */
  int getSize( Feature feature ) throws FilterEvaluationException;

  /**
   * Returns the (evaluated) value of the font's CssParameter 'font-size'.
   * <p>
   * 
   * @param size
   *          font-size to be set
   */
  void setSize( int size );

  /**
   * Returns the (evaluated) value of the font's CssParameter 'font-color'.
   * <p>
   * 
   * @param feature
   *          specifies the <tt>Feature</tt> to be used for evaluation of the underlying 'sld:ParameterValueType'
   * @return the (evaluated) value of the parameter
   * @throws FilterEvaluationException
   *           if the evaluation fails
   */
  Color getColor( Feature feature ) throws FilterEvaluationException;

  /**
   * Sets the value of the font's CssParameter 'font-color'.
   * <p>
   * 
   * @param color
   *          the font-color to be set
   */
  void setColor( Color color );
}