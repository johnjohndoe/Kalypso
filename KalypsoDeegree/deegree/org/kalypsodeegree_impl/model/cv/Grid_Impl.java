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

import org.deegree.model.coverage.Grid;
import org.deegree.model.coverage.GridRange;

/**
 * 
 *
 * <p>-----------------------------------------------------------------------</p>
 *
 * @author Andreas Poth
 * @version $Revision$ $Date$
 * <p>
 */
public class Grid_Impl implements Grid {
    
    private GridRange gridRange = null;
    private String type         = null;
    private int dimension       = 0;
    
    public Grid_Impl(int dimension, String type, GridRange gridRange)
    {
        this.gridRange = gridRange;
        this.dimension = dimension;
        this.type = type;
    }

    /** returns the range of a grid
     *
     */
    public GridRange getGridRange() {
        return gridRange;
    }    
   
    public int getDimension() {
        return dimension;
    }
    
    public String getType() {
        return type;
    }
    
}
