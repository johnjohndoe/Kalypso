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
package org.deegree_impl.model.geometry;

import java.io.Serializable;

import org.deegree.model.geometry.*;

import org.opengis.cs.*;


/**
* default implementation of the GM_Boundary interface. The class is
* abstract because there isn't a boundary without a geometry type.
* Concrete implementations are <tt>GM_CurveBoundary</tt> or
* <tt>GM_SurfaceBoundary</tt> for example.
* 
* <p>------------------------------------------------------------</p>
* @version 5.6.2001
* @author Andreas Poth
* <p>
*/
abstract class GM_Boundary_Impl extends GM_Object_Impl implements GM_Boundary, Serializable {
    /** Use serialVersionUID for interoperability. */
    private final static long serialVersionUID = -6057663115928108209L;

    /**
     * the reference system
     */
    public GM_Boundary_Impl( CS_CoordinateSystem srs ) {
        super( srs );
    }

    /** A geometric object, which has no boundary
     *    is a cycle.
     */
    public boolean isCycle() {
        return true;
    }
}