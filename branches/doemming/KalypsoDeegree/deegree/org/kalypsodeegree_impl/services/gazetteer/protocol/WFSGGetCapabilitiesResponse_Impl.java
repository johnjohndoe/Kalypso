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
package org.deegree_impl.services.gazetteer.protocol;

import org.deegree.services.*;
import org.deegree.services.gazetteer.capabilities.WFSGCapabilities;
import org.deegree.services.gazetteer.protocol.*;

import org.deegree_impl.services.OGCWebServiceResponse_Impl;

import org.w3c.dom.Document;


/**
 *
 * <p>--------------------------------------------------------</p>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
class WFSGGetCapabilitiesResponse_Impl extends OGCWebServiceResponse_Impl
    implements WFSGGetCapabilitiesResponse {
    private WFSGCapabilities response = null;

    /**
     * constructor
     */
    WFSGGetCapabilitiesResponse_Impl( OGCWebServiceRequest request, Document exception, 
                                      WFSGCapabilities response ) {
        super( request, exception );
        setResponse( response );
    }

    /**
     * returns the capabilities 
     */
    public WFSGCapabilities getResponse() {
        return response;
    }

    /**
     * sets the capabilities 
     */
    public void setResponse( WFSGCapabilities response ) {
        this.response = response;
    }

    /**
     *
     *
     * @return   
     */
    public String toString() {
        String ret = this.getClass().getName() + ":\n";
        ret += ( "response: " + response + "\n" );
        return ret;
    }
}
/*
 * Changes to this class. What the people haven been up to:
 *
 * $Log: 
 */
