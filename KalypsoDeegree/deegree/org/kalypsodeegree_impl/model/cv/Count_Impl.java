/*----------------    FILE HEADER  ------------------------------------------
 
This file is part of deegree (Java Framework for Geospatial Solutions).
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

package org.deegree_impl.model.cv;

import org.deegree.model.coverage.Count;

/**
* implements a count value of a histogram
*
* <p>-----------------------------------------------------------------------</p>
*
* @author Andreas Poth
* @version $Revision$ $Date$
* <p>
*/
public class Count_Impl implements Count {
    
    private int count       = 0;
    private double index    = 0; 
    private double percent  = 0;
    
    public Count_Impl(double index, int count, double percent) {
        this.count = count;
        this.index = index;
        this.percent = percent;
    }

    /** returns the absolut value of the histogram at the index position
     *
     */
    public int getCount() {
        return count;
    }    
   
    /** returns the index position of the value at the histogram
     *
     */
    public double getIndex() {
        return index;
    }    

    /** returns the relative value of the histogram at the index position
     *
     */
    public double getPercent() {
        return percent;
    }
    
    public String toString()
    {
        String ret = "Count_Impl: \n";
        ret += "count: " + count + "\n";
        ret += "percent: " + percent + "\n";
        ret += "index: " + index + "\n";
        return ret;
    }
    
}
