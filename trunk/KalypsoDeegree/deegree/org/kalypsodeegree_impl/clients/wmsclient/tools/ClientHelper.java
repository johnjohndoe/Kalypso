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
package org.deegree_impl.clients.wmsclient.tools;

import java.net.*;

import java.util.*;

import org.deegree.services.capabilities.*;
import org.deegree.services.wms.capabilities.*;

import org.deegree_impl.tools.NetWorker;


/**
 * 
 *
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 */
public class ClientHelper {
    /**
     * returns the http-get URL where to send a GetMap too.
     *
     * @param capa WMSCapabilities
     *
     * @return 
     */
    public synchronized static String getGetMapURL( WMSCapabilities capa ) {
        Request cReq = capa.getCapability().getRequest();
        DCPType dcp = cReq.getOperation( Operation.GETMAP ).getDCPTypes()[0];
        HTTP http = (HTTP)dcp.getProtocol();
        URL url = http.getGetOnlineResources()[0];
        String tmp = NetWorker.url2String( url );

        if ( !tmp.endsWith( "?" ) ) {
            tmp = tmp + "?";
        }

        return tmp;
    }
    
    /**
     * returns the http-get URL where to send a GetMap too.
     *
     * @param capa WMSCapabilities
     *
     * @return 
     */
    public synchronized static String getGetFeatureInfoURL( WMSCapabilities capa ) {
        Request cReq = capa.getCapability().getRequest();
        DCPType dcp = cReq.getOperation( Operation.GETFEATUREINFO ).getDCPTypes()[0];
        HTTP http = (HTTP)dcp.getProtocol();
        URL url = http.getGetOnlineResources()[0];
        String tmp = NetWorker.url2String( url );

        if ( !tmp.endsWith( "?" ) ) {
            tmp = tmp + "?";
        }

        return tmp;
    }

    /**
     * returns a list of all data layers available from the WMS which capabilities
     * are submitted to the method
     *
     * @param capa WMSCapabilities
     *
     * @return 
     */
    public synchronized static Layer[] getLayers( WMSCapabilities capa ) {
        ArrayList list = new ArrayList();
        Capability capability = capa.getCapability();
        Layer layer = capability.getLayer();

        if ( layer.getName() != null ) {
            list.add( layer );
        }
        
        list = getLayers( layer, list );        
        
        return (Layer[])list.toArray( new Layer[ list.size() ] );
    }

    /**
     *
     *
     * @param layer 
     * @param list 
     *
     * @return 
     */
    private static ArrayList getLayers( Layer layer, ArrayList list ) {
        
        Layer[] layers = layer.getLayer();
        if ( layers != null ) {
            for (int i = 0; i < layers.length; i++) {
                //if ( layers[i].getName() != null ) {
                    list.add( layers[i] );
                //} 
                list = getLayers( layers[i], list );
            }
        }
        return list;
    }
}