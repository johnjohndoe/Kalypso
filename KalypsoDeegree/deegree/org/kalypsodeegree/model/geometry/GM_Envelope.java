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
* a boundingbox as child of a GM_Polygon isn't part
* of the iso19107 spec but it simplifies the geometry handling
* within jago
*
*/
public interface GM_Envelope {
    /**
     * @link aggregationByValue
     * @clientCardinality 2
     */
    /*#GM_Position lnkGM_position;*/

   /**
    * returns the width of bounding box
    */
    double getWidth();
   /**
    * returns the height of bounding box
    */
    double getHeight();

   /**
    * returns the minimum coordinates of bounding box
    */
    GM_Position getMin();
   
   /**
    * returns the maximum coordinates of bounding box
    */
    GM_Position getMax();

   /**
    * returns true if the bounding box contains the submitted position
    */
    boolean contains(GM_Position position);

   /**
    * returns true if this envelope intersects the submitted envelope
    */
    boolean intersects(GM_Envelope bb);


   /**
    * returns true if all positions of the submitted
    * bounding box are within this bounding box
    */
    boolean contains(GM_Envelope bb);

   /**
    * returns a new GM_Envelope object representing the intersection of this
    * GM_Envelope with the specified GM_Envelope.
    */
    GM_Envelope createIntersection(GM_Envelope bb);
    
    /**
     * merges two GM_Envelops and returns the minimum envelope containing
     * both.
     * 
     * @return
     */
    GM_Envelope merge(GM_Envelope envelope);
    
    /**
     * creates a new envelope 
     */
    GM_Envelope getBuffer(double b);
    
}
