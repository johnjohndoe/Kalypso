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

package org.deegree.model.geometry;

import java.util.Iterator;

/**
 * 
 * This interface defines the basis functionallity of all geometry aggregations.
 * it will be specialized for the use of primitive, and solid geometries.
 * 
 * <p>
 * -----------------------------------------------------
 * </p>
 * 
 * @author Andreas Poth
 * @version $Revision$ $Date$
 *          <p>
 */

public interface GM_Aggregate extends GM_Object
{

  /**
   * returns the number of GM_Object within the aggregation
   */
  int getSize();

  /**
   * merges two aggregation.
   * 
   * @exception GM_Exception
   *              a GM_Exception will be thrown if the submitted isn't the same
   *              type as the recieving one.
   */
  void merge( GM_Aggregate aggregate ) throws GM_Exception;

  /**
   * adds an GM_Object to the aggregation
   */
  void add( GM_Object gmo );

  /**
   * inserts a GM_Object in the aggregation. all elements with an index equal or
   * larger index will be moved. if index is larger then getSize() - 1 an
   * exception will be thrown.
   * 
   * @param gmo
   *          GM_Object to insert.
   * @param index
   *          position where to insert the new GM_Object
   */
  void insertObjectAt( GM_Object gmo, int index ) throws GM_Exception;

  /**
   * sets the submitted GM_Object at the submitted index. the element at the
   * position <code>index</code> will be removed. if index is larger then
   * getSize() - 1 an exception will be thrown.
   * 
   * @param gmo
   *          GM_Object to set.
   * @param index
   *          position where to set the new GM_Object
   */
  void setObjectAt( GM_Object gmo, int index ) throws GM_Exception;

  /**
   * removes the submitted GM_Object from the aggregation
   * 
   * @return the removed GM_Object
   */
  GM_Object removeObject( GM_Object gmo );

  /**
   * removes the GM_Object at the submitted index from the aggregation. if index
   * is larger then getSize() - 1 an exception will be thrown.
   * 
   * @return the removed GM_Object
   */
  GM_Object removeObjectAt( int index ) throws GM_Exception;

  /**
   * removes all GM_Object from the aggregation.
   */
  void removeAll();

  /**
   * returns the GM_Object at the submitted index.
   */
  GM_Object getObjectAt( int index );

  /**
   * returns all GM_Objects as array
   */
  GM_Object[] getAll();

  /**
   * returns true if the submitted GM_Object is within the aggregation
   */
  boolean isMember( GM_Object gmo );

  /**
   * returns the aggregation as an iterator
   */
  Iterator getIterator();

}