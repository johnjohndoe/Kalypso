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

import org.deegree.model.coverage.SpatialExtent;
import org.deegree.model.coverage.ExtentType;

/**
 * The <tt>SpatialExtent</tt> lists the bounds along each of the spatial 
 * dimensions within which coverages may be requested from a coverage layer
 *
 * <p>-----------------------------------------------------------------------</p>
 *
 * @author Andreas Poth
 * @version $Revision$ $Date$
 * <p>
 */
class SpatialExtent_Impl implements SpatialExtent {
    
    private String crs          = null;
    private ExtentType xExtent  = null;
    private ExtentType yExtent  = null;
    private ExtentType zExtent  = null;
    
    SpatialExtent_Impl(String crs, ExtentType xExtent, ExtentType yExtent,
                       ExtentType zExtent)
    {
        this.crs = crs;
        this.xExtent = xExtent;
        this.yExtent = yExtent;
        this.zExtent = zExtent;
    }

    /** returns the CRS name the spatial extent is defined
     *
     */
    public String getCRS() {
        return crs;
    }    
    
    /** returns the x-value of the extent
     *
     */
    public ExtentType getXExtent() {
        return xExtent;
    }
    
    /** returns the y-value of the extent
     *
     */
    public ExtentType getYExtent() {
        return yExtent;
    }
    
    /** returns the z-value of the extent
     *
     */
    public ExtentType getZExtent() {
        return zExtent;
    }
    
}
