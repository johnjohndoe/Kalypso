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
* Defining the boundary of a surface. The surface boundary is defined as
* ring surounding the exterior boundary of the surface and the rings
* surounding each interior ring of the surface.
*
* <p>-----------------------------------------------------</p>
*
* @author Axel Schaefer
* @version $Revision$ $Date$
* <p>
*/ 

public interface GM_SurfaceBoundary extends GM_PrimitiveBoundary {
	/**
     * @clientCardinality 0..* 
     */
    /*#GM_Ring lnkGM_Ring;*/
    
    /* as
     * GM_SurfaceBoundary::exterior[0,1] : GM_Ring;
     * GM_SurfaceBoundary::interior[0..*] : GM_Ring;
     */
     
    /*
     * A GM_SurfaceBoundary consists of some number of GM_Rings,
     * corresponding to the various components of its boundary.
     * In the normal 2D case, one of these rings is distinguished
     * as being the exterior boundary.
     * In a general manifold this is not always possible, in which case
     * all boundaries shall be listed as interior boundaries, and the
     * exterior will be empty.
     */
    
	/*
	 * get the exterior ring
	 */	    
    public GM_Ring getExteriorRing();
    	
	/*
	 * gets the interior ring(s)
	 */              
    public GM_Ring[] getInteriorRings();
		
	
}
