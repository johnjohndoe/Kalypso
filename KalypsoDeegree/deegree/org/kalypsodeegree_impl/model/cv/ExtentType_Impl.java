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
package org.deegree_impl.model.cv;

import org.deegree.model.coverage.ExtentType;

/**
 * describes the common extent elements 
 *
 * <p>-----------------------------------------------------------------------</p>
 *
 * @author Andreas Poth
 * @version $Revision$ $Date$
 * <p>
 */
class ExtentType_Impl implements ExtentType {
    
    private Object default_     = null;
    private double min          = -9E99;
    private double max          = -9E99;
    private double resolution   = -9E99;
    private String uom          = null;
    private double value        = -9E99;
    
    public ExtentType_Impl(double min, double max, double resolution, String uom)
    {
        this.min = min;
        this.max = max;
        this.resolution = resolution;
        this.uom = uom;
    }
    
    public ExtentType_Impl(double value, String uom)
    {
        this.value = value;
        max = value;
        min = value;
        resolution = 0;
        this.uom = uom;
    }

    public Object getDefault() {
        return default_;
    }    
    
    /** returns the maximum of the extent interval
     *
     */
    public double getMax() {
        return max;
    }
    
    /** returns the minimum of the extent interval
     *
     */
    public double getMin() {
        return min;
    }
    
    /** returns the resolution of the extent interval
     *
     */
    public double getResolution() {
        return resolution;
    }
    
    /** return the units of measure in which it expresses time intervals or instances
     *
     */
    public String getUOM() {
        return uom;
    }
    
    /** returns the value of the extent if its a single value extent. in this case
     * getMin(), getMax() and getResolution() will return -9E99
     *
     */
    public double getValue() {
        return value;
    }
    
}
