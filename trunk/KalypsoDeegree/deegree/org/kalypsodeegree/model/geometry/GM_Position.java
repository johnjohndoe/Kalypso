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
* A sequence of decimals numbers which when written on a width are a sequence of
* coordinate positions. The width is derived from the CRS or coordinate dimension
* of the container.
*
* <p>-----------------------------------------------------------------------</p>
* @version 5.6.2001
* @author Andreas Poth
* <p>
*/
public interface GM_Position {
    /**
     * returns the x-value of the point
     */	
	public double getX();

    /**
     * returns the y-value of the point
     */	     
    public double getY();
    
    /**
     * returns the z-value of the point
     */	     
    public double getZ(); 

    /**
    * returns the x- and y-value of the point as a two dimensional
    * array the first field contains the x- the second field the
    * y-value.
    */ 
    public double[] getAsArray();

   /**
    * translates the coordinates of the position. the first coordinate
    * of the position will be translated by the first field of <tt>d</tt>
    * the second coordinate by the second field of <tt>d</tt> and so on...
    */
    public void translate(double[] d);
}
