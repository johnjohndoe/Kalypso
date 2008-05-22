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

import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.model.feature.Feature;

/**
 * The ParameterValueType element is a combination of text nodes and Filter-Expression element nodes.
 * <p>
 * 
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public interface ParameterValueType
{

  /**
   * Returns the contents (mix of <tt>String</tt>/<tt>Expression</tt> -objects) of this
   * <tt>ParameterValueType</tt>.
   * <p>
   * 
   * @return mix of <tt>String</tt> and <tt>Expression</tt> -objects
   */
  Object[] getComponents();

  /**
   * Sets the contents (mix of <tt>String</tt>/<tt>Expression</tt> -objects) of this <tt>ParameterValueType</tt>.
   * <p>
   * 
   * @param components
   *          mix of <tt>String</tt>/<tt>Expression</tt> -objects
   */
  void setComponents( Object[] components );

  /**
   * Concatenates a component (a<tt>String</tt> or an <tt>Expression</tt> -object) to this
   * <tt>ParameterValueType</tt>.
   * <p>
   * 
   * @param component
   *          either a <tt>String</tt> or an <tt>Expression</tt> -object
   */
  void addComponent( Object component );

  /**
   * Removes a component (a<tt>String</tt> or an <tt>Expression</tt> -object) from this <tt>ParameterValueType</tt>.
   * <p>
   * 
   * @param component
   *          either a <tt>String</tt> or an <tt>Expression</tt> -object
   */
  void removeComponent( Object component );

  /**
   * Returns the actual <tt>String</tt> value of this object. Expressions are evaluated according to the given
   * <tt>Feature</tt> -instance.
   * <p>
   * 
   * @param feature
   *          used for the evaluation of the underlying 'wfs:Expression'-elements
   * @return the evaluated extual representation, ready to be printed
   * @throws FilterEvaluationException
   *           if the evaluation fails
   */
  String evaluate( Feature feature ) throws FilterEvaluationException;
}