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
package org.deegree_impl.clients.wmsclient.control;

import java.net.URL;

import org.deegree_impl.clients.wmsclient.configuration.WMSClientConfiguration;
import org.deegree_impl.clients.wmsclient.configuration.WMSClientConfigurationFactory;
import org.deegree_impl.enterprise.control.ApplicationHandler;


/**
 * 
 *
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 */
public class MapApplicationHandler extends ApplicationHandler {
    private static WMSClientConfiguration defaultConfig = null;

    /**
     * Creates a new MapApplicationHandler object.
     *
     * @param controllerFile 
     * @param clientConfig 
     *
     * @throws Exception 
     */
    public MapApplicationHandler( String controllerFile, String clientConfig )
                          throws Exception {
        super( controllerFile );

        URL url = new URL( clientConfig );
        WMSClientConfigurationFactory fac = new WMSClientConfigurationFactory();
        defaultConfig = fac.createWMSClientConfiguration( url, null );
    }    
    
    /**
     * returns the default configuration of the WMS client
     */
    public static WMSClientConfiguration getDefaultClientConfiguration() {
        return defaultConfig;
    }
    
    /**
     * sets the default configuration for the map cliet
     */
    public static void setDefaultClientConfiguration(WMSClientConfiguration defaultConfig) {
        MapApplicationHandler.defaultConfig = defaultConfig;
    }
}