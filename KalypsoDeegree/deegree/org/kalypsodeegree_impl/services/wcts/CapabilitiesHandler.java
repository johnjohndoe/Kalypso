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
package org.deegree_impl.services.wcts;

import java.io.BufferedReader;
import java.io.FileReader;

import org.deegree.services.Handler;
import org.deegree.services.OGCWebServiceEvent;
import org.deegree.services.OGCWebServiceException;
import org.deegree.services.wcts.protocol.GetCapabilitiesRequest;
import org.deegree.services.wcts.protocol.GetCapabilitiesResponse;
import org.deegree_impl.services.OGCWebServiceEvent_Impl;
import org.deegree_impl.services.OGCWebServiceException_Impl;
import org.deegree_impl.services.wcts.protocol.WCTS_ProtocolFactory;


/**
 * This class handles the GetCapabilities-request and creates the response
 * which includes the Capabilities of the service.
 *
 * <p>--------------------------------------------------------------------</p>
* @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version 2002-08-07
 */
class CapabilitiesHandler implements Handler {
    private String FILENAME = null;
    
    public CapabilitiesHandler() {}
    
    public CapabilitiesHandler(String capaFile) {
        FILENAME = capaFile;
    }

    /**
     * handles a request against an OGC web service
     */
    public void handleRequest( OGCWebServiceEvent request ) {
        GetCapabilitiesResponse response = null;
        GetCapabilitiesRequest req = (GetCapabilitiesRequest)request.getRequest();

        // Checks the Version-attribute
        if ( !req.getVersion().equals( "1.0.0" ) ) {
            try {
                OGCWebServiceException exc = new OGCWebServiceException_Impl( getClass().toString(), 
                                                                              "wrong version: " + 
                                                                              req.getVersion() );

                // responseobjekt erzeugen
                response = WCTS_ProtocolFactory.createGetCapabilitiesResponse( req, exc, null );
            } catch ( Exception ex ) {
                System.out.println( ex );
            }
            // Checks the Service-attribute
        } else if ( !req.getService().equals( "WCTS" ) ) {
            try {
                OGCWebServiceException exc = new OGCWebServiceException_Impl( getClass().toString(), 
                                                                              "wrong service: " + 
                                                                              req.getService() );

                // responseobjekt erzeugen
                response = WCTS_ProtocolFactory.createGetCapabilitiesResponse( req, exc, null );
            } catch ( Exception ex ) {
                System.out.println( ex );
            }
            // Makes the GetCapabilitiesResponse from a GetCapabilities document
        } else {
            try {
                BufferedReader reader = new BufferedReader( new FileReader( FILENAME ) );

                StringBuffer sb = new StringBuffer( 60000 );
                String line = null;

                while ( ( line = reader.readLine() ) != null ) {
                    sb.append( line.trim() );
                }

                reader.close();

                String capabilities = sb.toString();

                // responseobjekt erzeugen
                response = WCTS_ProtocolFactory.createGetCapabilitiesResponse( req, null, 
                                                                               capabilities );
            } catch ( Exception ex ) {
                System.out.println( "Fatal error in CapabilitiesHandler: " + 
                                    "GetCapabilitiesResponse isn't created: " + ex );
            }
        }

        OGCWebServiceEvent ogcResp = new OGCWebServiceEvent_Impl( this, response, "-" );
        request.getDestination().write( ogcResp );
    }

    /**
     * handles the response of an OGC web service
     */
    public void handleResponse( OGCWebServiceEvent response ) {
    }

    /**
     * returns true if the handler is interested in a event
     */
    public boolean isInterested( OGCWebServiceEvent event ) {
        return true;
    }

    /**
     * registers a Handler so this Handler is able to act as a proxy
     * to the registered handler
     */
    public void registerHandler( Handler handler ) {
    }

    /**
     * @see CapabilitiesHandler#registerHandler(Handler)
     */
    public void removeHandler( Handler handler ) {
    }
}