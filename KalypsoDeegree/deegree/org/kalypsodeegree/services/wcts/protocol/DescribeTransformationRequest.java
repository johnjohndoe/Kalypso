/*----------------    FILE HEADER  ------------------------------------------

This file is part of Deegree.
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

Markus Mueller
Department of Geography
University of Bonn
Meckenheimer Allee 166
53115 Bonn
Germany
E-Mail: mm@giub.uni-bonn.de

 ---------------------------------------------------------------------------*/

package org.deegree.services.wcts.protocol;

import org.deegree.services.OGCWebServiceRequest;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * The only request not defined as mandatory supplies the description 
 * of the transformation of coordinates from a spatial reference system to 
 * another. The structure of the requests essentially corresponds to that of 
 * the IsTransformable Requests. The transformation steps, which 
 * are necessary, are inquired in order to transform a coordinate from the 
 * SourceCRS into the TargetCRS. 
 * <p>----------------------------------------------------------------------</p>
* @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version 2002-07-10
 */

public interface DescribeTransformationRequest extends OGCWebServiceRequest {
	
	/**
     *  gets the format
     */
    public String getFormat(); 
	
	/**
	* gets the version
	*/
	public String getVersion();
	
	
	/**
     * gets the SourceCRS
     */
	public CS_CoordinateSystem getSourceCRS();
	
	
	/**
     * gets the DestinationCRS
     */
	public CS_CoordinateSystem getDestinationCRS();
	
}

 