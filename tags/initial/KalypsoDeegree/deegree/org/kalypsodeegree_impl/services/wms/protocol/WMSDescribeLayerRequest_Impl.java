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
package org.deegree_impl.services.wms.protocol;

import java.util.HashMap;

import org.deegree.services.wms.protocol.WMSDescribeLayerRequest;
import org.deegree_impl.services.OGCWebServiceRequest_Impl;


/**
 *
 *
 * <p>---------------------------------------------------------------------</p>
 *
 * @author Katharina Lupp <a href="mailto:k.lupp@web.de">Katharina Lupp</a>
 * @version $Revision$ $Date$
 */
public class WMSDescribeLayerRequest_Impl extends OGCWebServiceRequest_Impl
    implements WMSDescribeLayerRequest {
    /**
     * Creates a new WMSDescribeLayerRequest_Impl object.
     *
     * @param version 
     * @param request 
     * @param id 
     * @param vendorSpecificParameter 
     */
    WMSDescribeLayerRequest_Impl( String version, String id, HashMap vendorSpecificParameter ) {
        super( "DescribeLayer", "WMS", version, id, vendorSpecificParameter );
    }
}
