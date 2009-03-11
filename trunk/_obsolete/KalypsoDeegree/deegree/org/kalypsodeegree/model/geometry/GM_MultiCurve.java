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

package org.kalypsodeegree.model.geometry;

/**
 * 
 * The interface defines the access to a aggregations of <tt>GM_Curve</tt> objects.
 * 
 * <p>
 * -----------------------------------------------------
 * </p>
 * 
 * @author Andreas Poth
 * @version $Revision$ $Date$
 *          <p>
 */
public interface GM_MultiCurve extends GM_MultiPrimitive
{
  /**
   * @link aggregation
   * @clientCardinality 0..*
   */
  /* #GM_Curve lnkGM_Curve; */

  /**
   * adds a GM_Curve to the aggregation
   */
  public void addCurve( GM_Curve gmc );

  /**
   * inserts a GM_Curve in the aggregation. all elements with an index equal or larger index will be moved. if index is
   * larger then getSize() - 1 or smaller then 0 or gmc equals null an exception will be thrown.
   * 
   * @param gmc
   *          GM_Curve to insert.
   * @param index
   *          position where to insert the new GM_Curve
   */
  public void insertCurveAt( GM_Curve gmc, int index ) throws GM_Exception;

  /**
   * sets the submitted GM_Curve at the submitted index. the element at the position <code>index</code> will be
   * removed. if index is larger then getSize() - 1 or smaller then 0 or gmc equals null an exception will be thrown.
   * 
   * @param gmc
   *          GM_Curve to set.
   * @param index
   *          position where to set the new GM_Curve
   */
  public void setCurveAt( GM_Curve gmc, int index ) throws GM_Exception;

  /**
   * removes the submitted GM_Curve from the aggregation
   * 
   * @return the removed GM_Curve
   */
  public GM_Curve removeCurve( GM_Curve gmc );

  /**
   * removes the GM_Curve at the submitted index from the aggregation. if index is larger then getSize() - 1 or smaller
   * then 0 an exception will be thrown.
   * 
   * @return the removed GM_Curve
   */
  public GM_Curve removeCurveAt( int index ) throws GM_Exception;

  /**
   * returns the GM_Curve at the submitted index. if index is larger then getSize() - 1 or smaller then 0 an exception
   * will be thrown.
   */
  public GM_Curve getCurveAt( int index );

  /**
   * returns all GM_Curves as array
   */
  public GM_Curve[] getAllCurves();

}