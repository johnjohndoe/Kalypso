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

/**
*
* The interface defines the access to a aggregations of 
* <tt>GM_Point</tt> objects.
*
* <p>-----------------------------------------------------</p>
*
* @author Andreas Poth
* @version $Revision$ $Date$
* <p>
*/ 
public interface GM_MultiPoint extends GM_MultiPrimitive {
    /**
     * @link aggregation
     * @clientCardinality 0..* 
     */
    /*#GM_Point lnkGM_OrientablePrimitive;*/
    
   /**
    * adds a GM_Point to the aggregation 
    */	
    public void addPoint(GM_Point gmp);
    
   /**
    * inserts a GM_Point into the aggregation. all elements with an index 
    * equal or larger index will be moved. if index is
    * larger then getSize() - 1 or smaller then 0 or gmp equals null 
    * an exception will be thrown.
    *
    * @param gmp GM_Point to insert.     
    * @param index position where to insert the new GM_Point
    */ 
    public void insertPointAt(GM_Point gmp, int index) throws GM_Exception;
    
   /**
    * sets the submitted GM_Point at the submitted index. the element
    * at the position <code>index</code> will be removed. if index is
    * larger then getSize() - 1 or smaller then 0 or gmp equals null 
    * an exception will be thrown.
    *
    * @param gmp GM_Point to set.     
    * @param index position where to set the new GM_Point
    */ 
    public void setPointAt(GM_Point gmp, int index) throws GM_Exception;
    
   /**
    * removes the submitted GM_Point from the aggregation
    *
    * @return the removed GM_Point
    */ 
    public GM_Point removePoint(GM_Point gmp) ;
    
   /**
    * removes the GM_Point at the submitted index from the aggregation.
    * if index is larger then getSize() - 1 or smaller then 0 
    * an exception will be thrown.
    *
    * @return the removed GM_Point
    */ 
    public GM_Point removePointAt(int index) throws GM_Exception;       
    
   /**
    * returns the GM_Point at the submitted index. 
    */ 
    public GM_Point getPointAt(int index);
    
   /**
    * returns all GM_Points as array
    */ 
    public GM_Point[] getAllPoints();
    
  
}
