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
package org.deegree_impl.services.wts.protocol;

import java.util.HashMap;

import org.deegree.services.WebServiceException;
import org.deegree.services.wts.protocol.WTSGetCapabilitiesRequest;
import org.deegree_impl.services.OGCWebServiceRequest_Impl;


/**
 * This interface desribes the access to the parameters common to a OGC
 * GetCapabilities request. It inherits three accessor methods from the
 * general OGC web service request interface.
 */
public class WTSGetCapabilitiesRequest_Impl extends OGCWebServiceRequest_Impl
    implements WTSGetCapabilitiesRequest {
    private String updateSequence = null;

    /**
     * Creates a new WTSGetCapabilitiesRequest_Impl object.
     *
     * @param version 
     * @param id 
     * @param vendorSpecificParameter 
     */
    WTSGetCapabilitiesRequest_Impl(String version, String id, HashMap vendorSpecificParameter) {
        super("GetCapabilities", "WTS", version, id, vendorSpecificParameter);
    }

    /**
     * The optional UPDATESEQUENCE parameter is for maintaining cache consistency.
     * Its value can be an integer, a timestamp in [ISO 8601:1988(E)] format, or
     * any other number or string. The server may include an UpdateSequence value
     * in its Capabilities XML. If present, this value should be increased when
     * changes are made to the Capabilities (e.g., when new maps are added to the
     * service). The server is the sole judge of lexical ordering sequence. The
     * client may include this parameter in its GetCapabilities request.
     */
    public String getUpdateSequence() {
        return updateSequence;
    }

    /** returns the URI of a HTTP GET request. If the request doesn't support
     * HTTP GET a <tt>WebServiceException</tt> will be thrown
     *
     */
    public String getRequestParameter() throws WebServiceException {
        return null;
    }
}