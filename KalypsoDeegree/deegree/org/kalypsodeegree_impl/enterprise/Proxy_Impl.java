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
package org.deegree_impl.enterprise;

import java.util.Properties;

import org.deegree.enterprise.Proxy;


/**
 * encapsulates proxy informations and offers a method for setting and 
 * unsetting the proxy
 *
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 */
public class Proxy_Impl implements Proxy {
    private String proxyHost = null;
    private String proxyPort = null;

    /** Creates a new instance of Proxy_Impl */
    public Proxy_Impl( String proxyHost, String proxyPort ) {
        this.proxyHost = proxyHost;
        this.proxyPort = proxyPort;
    }

    /**
    * retuns the proxy host definition
    *
    * @return 
    */
    public String getProxyHost() {
        return proxyHost;
    }

    /**
     * returns the proxy port definition
     *
     * @return 
     */
    public String getProxyPort() {
        return proxyPort;
    }

    /**
     * sets or unsets the proxy by writing the proxyHost and the proxyPort
     * to the system properties.
     *
     * @param proxySet 
     */
    public void setProxy( boolean proxySet ) {
        Properties sysProperties = System.getProperties();

        // Specify proxy settings
        sysProperties.put( "proxyHost", proxyHost );
        sysProperties.put( "proxyPort", proxyPort );
        sysProperties.put( "proxySet", "" + proxySet );
    }
    
    public String toString() {
        String ret = "proxyHost: " + proxyHost + "\n";
        ret += "proxyPort: " + proxyPort;
        return ret;
    }
}