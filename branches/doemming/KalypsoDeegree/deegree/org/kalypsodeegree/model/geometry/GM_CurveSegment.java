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

import org.opengis.cs.CS_CoordinateSystem;

/**
 *
 * The interface defines the root of each unit building Curves
 *
 * <p>-----------------------------------------------------</p>
 *
 * @author Andreas Poth
 * @version $Revision$ $Date$
 * <p>
 */
public interface GM_CurveSegment extends GM_GenericCurve {
    
    /**
     * returns the number of points building the curve segment
     */
    int getNumberOfPoints();
    
    /**
     * returns all positions of the segement as array of GM_Point
     */
    GM_Position[] getPositions();
    
    /**
     * returns the curve position at the submitted index
     */
    GM_Position getPositionAt(int index);
    
    /**
     * reverses the direction of the curvesegment
     */
    void reverse();
    
    /**
     * returns the coordinate system of the curve segment
     */
    CS_CoordinateSystem getCoordinateSystem();
    
    /**
     * The Boolean valued operation "intersects" shall return TRUE if this GM_Object
     * intersects another GM_Object. Within a GM_Complex, the GM_Primitives do not
     * intersect one another. In general, topologically structured data uses shared
     * geometric objects to capture intersection information.
     */
    boolean intersects(GM_Object gmo);
    
    /**
     * The Boolean valued operation "contains" shall return TRUE if this GM_Object
     * contains another GM_Object.
     */
    boolean contains(GM_Object gmo);
    
    /**
     * @link aggregationByValue
     * @clientCardinality 2..*
     */
    /*#GM_Position lnkGM_Position;*/
}
