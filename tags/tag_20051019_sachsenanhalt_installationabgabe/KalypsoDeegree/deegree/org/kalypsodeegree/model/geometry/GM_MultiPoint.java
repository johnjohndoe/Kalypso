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

package org.kalypsodeegree.model.geometry;

/**
 * 
 * The interface defines the access to a aggregations of <tt>GM_Point</tt> objects.
 * 
 * <p>
 * -----------------------------------------------------
 * </p>
 * 
 * @author Andreas Poth
 * @version $Revision$ $Date$
 *          <p>
 */
public interface GM_MultiPoint extends GM_MultiPrimitive
{
  /**
   * @link aggregation
   * @clientCardinality 0..*
   */
  /* #GM_Point lnkGM_OrientablePrimitive; */

  /**
   * adds a GM_Point to the aggregation
   */
  public void addPoint( GM_Point gmp );

  /**
   * inserts a GM_Point into the aggregation. all elements with an index equal or larger index will be moved. if index
   * is larger then getSize() - 1 or smaller then 0 or gmp equals null an exception will be thrown.
   * 
   * @param gmp
   *          GM_Point to insert.
   * @param index
   *          position where to insert the new GM_Point
   */
  public void insertPointAt( GM_Point gmp, int index ) throws GM_Exception;

  /**
   * sets the submitted GM_Point at the submitted index. the element at the position <code>index</code> will be
   * removed. if index is larger then getSize() - 1 or smaller then 0 or gmp equals null an exception will be thrown.
   * 
   * @param gmp
   *          GM_Point to set.
   * @param index
   *          position where to set the new GM_Point
   */
  public void setPointAt( GM_Point gmp, int index ) throws GM_Exception;

  /**
   * removes the submitted GM_Point from the aggregation
   * 
   * @return the removed GM_Point
   */
  public GM_Point removePoint( GM_Point gmp );

  /**
   * removes the GM_Point at the submitted index from the aggregation. if index is larger then getSize() - 1 or smaller
   * then 0 an exception will be thrown.
   * 
   * @return the removed GM_Point
   */
  public GM_Point removePointAt( int index ) throws GM_Exception;

  /**
   * returns the GM_Point at the submitted index.
   */
  public GM_Point getPointAt( int index );

  /**
   * returns all GM_Points as array
   */
  public GM_Point[] getAllPoints();

}