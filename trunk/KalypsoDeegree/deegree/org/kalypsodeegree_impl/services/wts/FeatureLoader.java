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
package org.deegree_impl.services.wts;

import java.io.StringReader;

import org.deegree.services.OGCWebServiceEvent;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.wts.configuration.WTSConfiguration;
import org.deegree.services.wts.protocol.WTSGetViewRequest;
import org.deegree_impl.services.OGCWebServiceEvent_Impl;
import org.deegree_impl.services.wfs.protocol.WFSProtocolFactory;
import org.deegree_impl.services.wts.configuration.WTSConfiguration_Impl;

/**
     * loader for feature in a seperat thread
     */
class FeatureLoader extends Thread {
    private WTSGetViewRequest request = null;
    private WTService_Impl parent = null;

    /**
     * Creates a new FeatureLoader object.
     *
     * @param request 
     * @param parent 
     */
    FeatureLoader( WTSGetViewRequest request, WTService_Impl parent ) {
        this.request = request;
        this.parent = parent;
    }

    /**
     *
     */
    public void run() {
        try {
            WTSConfiguration conf = WTSConfiguration_Impl.getInstance();

            if ( ( request.getFeatureCollections() != null ) && 
                     ( request.getFeatureCollections().length > 0 ) ) {
                String feat = request.getFeatureCollections()[0].getName();
                StringBuffer sb = new StringBuffer();
                sb.append( "<wfs:GetFeature outputFormat=\"GML2\" " );
                sb.append( "xmlns:gml=\"http://www.opengis.net/gml\" " );
                sb.append( "xmlns:wfs=\"http://www.opengis.net/namespaces/wfs\" >" );
                sb.append( "<wfs:Query typeName=\"" + feat + "\">" );
                sb.append( "</wfs:Query>" );
                sb.append( "</wfs:GetFeature>" );
                StringReader sr = new StringReader( sb.toString() );
                OGCWebServiceRequest wfsReq = WFSProtocolFactory.createRequest( "ENTITY"+this.toString(), sr );
                sr.close();
                OGCWebServiceEvent event = new OGCWebServiceEvent_Impl( this, wfsReq, "", parent );
                conf.getResponsibleService( feat ).doService( event );
            }
        } catch ( Exception e ) {
            e.printStackTrace();
        }
    }
}