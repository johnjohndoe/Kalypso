/*----------------    FILE HEADER  ------------------------------------------

This file is part of deegree
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
E-Mail: fitzke@giub.uni-bonn.de


 ---------------------------------------------------------------------------*/
package org.deegree_impl.model.gc;

import java.io.Serializable;

import org.opengis.gc.GC_GridRange;

/**
 * Specifies the range of valid coordinates for each dimension of the coverage.
 *
 * @version 1.00
 * @since   1.00
 */
public class GC_GridRange_Impl implements GC_GridRange, Serializable
{
    private int[] hi  = null;
    private int[] lo  = null;
        
    
    public GC_GridRange_Impl(int[]lo, int[] hi)
    {
        this.hi = hi;
        this.lo = lo;
    }
    
    /** The valid maximum exclusive grid coordinate.
     * The sequence contains a maximum value for each dimension of the grid coverage.
     *
     */
    public int[] getHi() {
        return hi;
    }
    
    /** The valid minimum inclusive grid coordinate.
     * The sequence contains a minimum value for each dimension of the grid coverage.
     * The lowest valid grid coordinate is zero.
     *
     */
    public int[] getLo() {
        return lo;
    }
    
}
