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
package org.deegree_impl.services.wms.capabilities;

import java.net.*;

import org.deegree.services.wms.capabilities.*;
import org.deegree.xml.Marshallable;
import org.deegree.xml.XMLTools;

import org.deegree_impl.tools.NetWorker;
import org.deegree.enterprise.Proxy;


/**
 * 
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider</a>
 * @version $Revision$
 */
class DeegreeParam_Impl implements DeegreeParam, Marshallable {
    private String rootDirectory = null;
    private URL defaultOnlineResource = null;
    private float mapQuality = 0.95f;
    private int cacheSize = 100; 
    private int maxLifeTime = 3600;
    private int requestTimeLimit = 15;
    private int maxMapWidth = 1000;
    private int maxMapHeight = 1000;   
    private String copyRight = "";
    private GazetteerParam gazetteer = null;
    private URL schemaLocation = null;
    private URL dtdLocation = null;
    private Proxy proxy = null;
   

    /** Creates a new instance of DeegreeParam_Impl */
    DeegreeParam_Impl( int cacheSize, int maxLifeTime, int requestTimeLimit, float mapQuality, 
                       URL defaultOnlineResource, String rootDirectory, int maxMapWidth,
                       int maxMapHeight, String copyRight, GazetteerParam gazetteer,
                       URL schemaLocation, URL dtdLocation, Proxy proxy) {
        this.cacheSize = cacheSize;
        this.maxLifeTime = maxLifeTime;
        this.requestTimeLimit = requestTimeLimit;
        this.mapQuality = mapQuality;
        this.defaultOnlineResource = defaultOnlineResource;
        this.rootDirectory = rootDirectory;
        this.maxMapHeight = maxMapHeight;
        this.maxMapWidth = maxMapWidth;
        this.copyRight = copyRight;
        this.gazetteer = gazetteer;
        this.schemaLocation = schemaLocation;
        this.dtdLocation = dtdLocation;
        this.proxy = proxy;
    }

    /** returns the cache size used for the WMS in MB. default is 100MB.
     * Datasources that are linked to WMS are not targeted by this value.
     *
     * @return cache size
     *
     */
    public int getCacheSize() {
        return cacheSize;
    }

    /** returns the default URL of the deegree WMS which is used as default if
     * no other URL is specified in the capabilities/configuration document if
     * required.
     *
     * @return URL
     *
     */
    public URL getDefaultOnlineResource() {
        return defaultOnlineResource;
    }

    /** returns the maximum life time of the internal processes (Threads) of
     * the deegree WMS. default is 3600 seconds. Datasources that are linked
     * to WMS are not targeted by this value.
     *
     * @return
     *
     */
    public int getMaxLifeTime() {
        return maxLifeTime;
    }

    /** returns the root directory of the deegree WMS installation, The directory
     * contains the WEB-INF directory and so the deployment descriptor for
     * publishing the WMS to the servlet engine.<p>
     * The WEB-INF directory contains a directory named xml that contains all
     * required XML-Documents that are not addressed with its absolut path in
     * the capabilities/configuration document.
     *
     * @return directory name
     *
     */
    public String getRootDirectory() {
        return rootDirectory;
    }

    /** returns the maximum time the perfoming of a request is allowed to take.
     * default is 15 seconds.
     *
     */
    public int getRequestTimeLimit() {
        return requestTimeLimit;
    }
    
    /** returns a copy right note to draw at the left side of the maps bottom
     *
     */
    public String getCopyRight() {
        return copyRight;
    }
    
    /** returns the quality of the map for none loss-less image formats. the
     * value ranges from 0 (lowest quality) to 1 (best quality)
     * <p>Default is 0.95
     */
    public float getMapQuality() {
        return mapQuality;
    }
    
    /** returns the maximum map height that can be requested. If the GetMap-Parameter
     * 'HEIGHT' extends max map width an exception shall be returned to the client.
     * <p>Default is 1000 
     */
    public int getMaxMapHeight() {
        return maxMapHeight;
    }
    
    /** returns the maximum map width that can be requested. If the GetMap-Parameter
     * 'WIDTH' extends max map width an exception shall be returned to the client.
     * <p>Default is 1000 
     */
    public int getMaxMapWidth() {
        return maxMapWidth;
    }
    
    /** returns the URL where to access the gazetteer service associated with the WMS
     *
     */
    public GazetteerParam getGazetteer() {
        return gazetteer;
    }
    
    /**
     * returns the URL where the sxm schema definition of the response to an
     * GetFeatureInfo request is located
     */
    public URL getSchemaLocation() {
        return schemaLocation;
    }  
    
    /**
     * returns the URL where the DTD defining the OGC WMS capabilities is located
     */
    public URL getDTDLocation() {
        return dtdLocation;
    }
    
    /**
     * returns the proxy used with the WMS. 
     */
    public Proxy getProxy() {
        return proxy;
    }    
    
    /**
     * Returns an XML representation of this object.
     */
    public String exportAsXML() {
        StringBuffer sb = new StringBuffer(1000);

        sb.append( "<DeegreeParam>" ).append( "<RootDirectory>" )
          .append( XMLTools.validateCDATA( rootDirectory ) ).append( "</RootDirectory>" )
          .append( "<DefaultOnlineResource xmlns:xlink=\"http://www.w3.org/1999/xlink\" xlink:type=\"simple\" xlink:href=\"" )
          .append( NetWorker.url2String (defaultOnlineResource) ).append( "\"/>" ).append( "<CacheSize>" )
          .append( cacheSize ).append( "</CacheSize>" ).append( "<MaxLifeTime>" )
          .append( maxLifeTime ).append( "</MaxLifeTime>" ).append( "<RequestTimeLimit>" )
          .append( requestTimeLimit ).append( "</RequestTimeLimit>" ).append( "<MapQuality>" )
          .append( mapQuality ).append( "</MapQuality>" ).append( "<MaxMapWidth>" )
          .append( maxMapWidth ).append( "</MaxMapWidth>" ).append( "<MaxMapHeight>" )
          .append( maxMapHeight ).append( "</MaxMapHeight>" );
        if ( gazetteer != null ) {
            sb.append( "<Gazetteer>" ).append( ((Marshallable)gazetteer).exportAsXML() )
               .append( "</Gazetteer>" );
        }
        if ( schemaLocation != null ) {
            sb.append("<SchemaLocation>")
              .append( "<OnlineResource xmlns:xlink=\"http://www.w3.org/1999/xlink\" xlink:type=\"simple\" xlink:href=\"" )
              .append( NetWorker.url2String(schemaLocation) ).append( "\"/>" )
              .append("</SchemaLocation>");
        }
        if ( schemaLocation != null ) {
            sb.append("<DTDLocation>")
              .append( "<OnlineResource xmlns:xlink=\"http://www.w3.org/1999/xlink\" xlink:type=\"simple\" xlink:href=\"" )
              .append( NetWorker.url2String(schemaLocation) ).append( "\"/>" )
              .append("</DTDLocation>");
        }
        if ( proxy != null ) {
            sb.append("<Proxy proxyHost='").append( proxy.getProxyHost() + "' " )
              .append( "proxyPort='" ).append( proxy.getProxyPort() + "' />" );
        }
        sb.append( "</DeegreeParam>" );

        return sb.toString();
    }              
    
}
