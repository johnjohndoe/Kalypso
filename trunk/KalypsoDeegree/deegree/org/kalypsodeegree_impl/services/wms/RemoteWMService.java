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
package org.deegree_impl.services.wms;

import com.sun.media.jai.codec.*;

import java.awt.image.*;

import java.io.*;

import java.net.*;

import java.util.*;

import javax.media.jai.JAI;
import javax.media.jai.RenderedOp;

import org.deegree.services.*;
import org.deegree.services.capabilities.*;
import org.deegree.services.wms.capabilities.*;
import org.deegree.services.wms.protocol.*;

import org.deegree_impl.services.*;
import org.deegree_impl.services.wms.capabilities.*;
import org.deegree_impl.services.wms.protocol.*;
import org.deegree_impl.tools.*;


/**
 * An instance of the class acts as a wrapper to a remote WMS.
 *
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 */
public class RemoteWMService extends OGCWebService_Impl {
    protected HashMap addresses = null;
    protected WMSCapabilities capabilities = null;

    /** Creates a new instance of RemoteWMService */
    public RemoteWMService( WMSCapabilities capabilities ) throws WebServiceException {
        this.capabilities = capabilities;
        addresses = new HashMap();

        Request request = capabilities.getCapability().getRequest();
        // get GetCapabilities operation address
        DCPType[] dcp = null;
        URL[] get = null;
        Operation operation = null;

        if ( capabilities.getVersion().equals( "1.0.0" ) ) {
            operation = request.getOperation( Operation.CAPABILITIES );
            dcp = operation.getDCPTypes();
            get = ( (HTTP)dcp[0].getProtocol() ).getGetOnlineResources();
            addresses.put( Operation.CAPABILITIES_NAME, get[0] );
        } else {
            operation = request.getOperation( Operation.GETCAPABILITIES );
            dcp = operation.getDCPTypes();
            get = ( (HTTP)dcp[0].getProtocol() ).getGetOnlineResources();
            addresses.put( Operation.GETCAPABILITIES_NAME, get[0] );
        }

        // get GetMap operation address
        if ( capabilities.getVersion().equals( "1.0.0" ) ) {
            operation = request.getOperation( Operation.MAP );
            dcp = operation.getDCPTypes();
            get = ( (HTTP)dcp[0].getProtocol() ).getGetOnlineResources();
            addresses.put( Operation.MAP_NAME, get[0] );
        } else {
            operation = request.getOperation( Operation.GETMAP );
            dcp = operation.getDCPTypes();
            get = ( (HTTP)dcp[0].getProtocol() ).getGetOnlineResources();
            addresses.put( Operation.GETMAP_NAME, get[0] );
        }

        // get GetFeatureInfo operation address
        if ( capabilities.getVersion().equals( "1.0.0" ) ) {
            operation = request.getOperation( Operation.FEATUREINFO );

            if ( operation != null ) {
                dcp = operation.getDCPTypes();
                get = ( (HTTP)dcp[0].getProtocol() ).getGetOnlineResources();
                addresses.put( Operation.FEATUREINFO_NAME, get[0] );
            }
        } else {
            operation = request.getOperation( Operation.GETFEATUREINFO );

            if ( operation != null ) {
                dcp = operation.getDCPTypes();
                get = ( (HTTP)dcp[0].getProtocol() ).getGetOnlineResources();
                addresses.put( Operation.GETFEATUREINFO_NAME, get[0] );
            }
        }

        // get GetLegendGraphic operation address
        operation = request.getOperation( Operation.GETLEGENDGRAPHIC );

        if ( operation != null ) {
            dcp = operation.getDCPTypes();
            get = ( (HTTP)dcp[0].getProtocol() ).getGetOnlineResources();
            addresses.put( Operation.GETLEGENDGRAPHIC_NAME, get[0] );
        }

        // get GetStyles operation address
        operation = request.getOperation( Operation.GETSTYLES );

        if ( operation != null ) {
            dcp = operation.getDCPTypes();
            get = ( (HTTP)dcp[0].getProtocol() ).getGetOnlineResources();
            addresses.put( Operation.GETSTYLES_NAME, get[0] );
        }

        // get PutStyles operation address
        operation = request.getOperation( Operation.PUTSTYLES );

        if ( operation != null ) {
            dcp = operation.getDCPTypes();
            get = ( (HTTP)dcp[0].getProtocol() ).getGetOnlineResources();
            addresses.put( Operation.PUTSTYLES_NAME, get[0] );
        }

        // get DescribeLayer operation address
        operation = request.getOperation( Operation.DESCRIBELAYER );

        if ( operation != null ) {
            dcp = operation.getDCPTypes();
            get = ( (HTTP)dcp[0].getProtocol() ).getGetOnlineResources();
            addresses.put( Operation.DESCRIBELAYER_NAME, get[0] );
        }
    }

    /**
     *
     *
     * @param request request to be performed
     */
    public synchronized void doService( OGCWebServiceEvent event ) throws WebServiceException {
        Debug.debugMethodBegin( this, "doService" );

        OGCWebServiceRequest request = event.getRequest();
        OGCWebServiceClient client = event.getDestination();

        if ( request instanceof WMSGetMapRequest ) {
            handleGetMap( (WMSGetMapRequest)request, client );
        } else if ( request instanceof WMSFeatureInfoRequest ) {
            handleFeatureInfo( (WMSFeatureInfoRequest)request, client );
        } else if ( request instanceof WMSGetCapabilitiesRequest ) {
            handleGetCapabilities( (WMSGetCapabilitiesRequest)request, client );
        } else if ( request instanceof WMSGetStylesRequest ) {
            handleGetStyles( (WMSGetStylesRequest)request, client );
        } else if ( request instanceof WMSPutStylesRequest ) {
            handlePutStyles( (WMSPutStylesRequest)request, client );
        } else if ( request instanceof WMSDescribeLayerRequest ) {
            handleDescribeLayer( (WMSDescribeLayerRequest)request, client );
        } else if ( request instanceof WMSGetLegendGraphicRequest ) {
            handleGetLegendGraphic( (WMSGetLegendGraphicRequest)request, client );
        }

        Debug.debugMethodEnd();
    }
    
    /**
     * the method performs the handling of the passed OGCWebServiceEvent directly 
     * and returns the result to the calling class/method
     *
     * @param request request (WMS, WCS, WFS, WCAS, WCTS, WTS, Gazetter) to perform
     *
     * @throws WebServiceException 
     */
    public OGCWebServiceResponse doService( OGCWebServiceRequest request ) throws WebServiceException {
        Debug.debugMethodBegin();        
        Debug.debugMethodEnd();
        throw new NoSuchMethodError( "doService(OGCWebServiceRequest)" );
    }

    /**
     * performs a GetMap request against the remote service. The result contains
     * the map decoded in the desired format as a byte array.
     *
     * @param request GetMap request
     * @return map (image) in the desired format as byte array
     */
    protected void handleGetMap( WMSGetMapRequest request, OGCWebServiceClient client )
                         throws WebServiceException {
        Debug.debugMethodBegin( this, "handleGetMap" );

        URL url = null;

        if ( request.getVersion().equals( "1.0.0" ) ) {
            url = (URL)addresses.get( Operation.MAP_NAME );
        } else {
            url = (URL)addresses.get( Operation.GETMAP_NAME );
        }

        String remoteAddress = NetWorker.url2String( url );
        String param = request.getRequestParameter();
        String us = remoteAddress + "?" + param;
        Debug.debugObject( "remote wms getmap", us );

        if ( capabilities.getVersion().compareTo( "1.0.0" ) <= 0 ) {
            us = StringExtend.replace( us, "TRANSPARENCY", "TRANSPARENT", false );
            us = StringExtend.replace( us, "GetMap", "map", false );
            us = StringExtend.replace( us, "image/", "", false );
        }

        new RemoteWMSHandler( request, client, us, param ) {
            public void run() {
                OGCWebServiceException exce = null;
                BufferedImage result = null;

                try {
                    URL ur = new URL( laddress );
                    // get map from the remote service                
                    NetWorker nw = new NetWorker( ur );
                    InputStream is = nw.getInputStream();

                    String contentType = nw.getContentType();
                    String[] tmp = StringExtend.toArray( contentType, ";", true );
                    for (int i = 0; i < tmp.length; i++) {
                        if ( tmp[i].indexOf( "image" ) > -1 ) {
                            contentType = tmp[i];
                            break;
                        } else {
                            contentType = tmp[0];
                        }
                    }

                    if ( MimeTypeMapper.isImageType( contentType ) && 
                         MimeTypeMapper.isKnownImageType( contentType ) ) {
                        MemoryCacheSeekableStream mcss = new MemoryCacheSeekableStream( is );
                        RenderedOp rop = JAI.create( "stream", mcss );
                        result = rop.getAsBufferedImage();
                        mcss.close();
                    } else {
                        // extract remote (error) message if the response
                        // contains a known mime type
                        String res = "";

                        if ( MimeTypeMapper.isKnownMimeType( contentType ) ) {
                            res = "; remote message: ";
                            res += getInputStreamContent( is );
                        }

                        exce = new OGCWebServiceException_Impl( "RemoteWMS:handleGetMap", 
                                                                "Response of the remote " + 
                                                                "WMS contains wrong content " + 
                                                                "type: " + contentType + 
                                                                ";request: " + lparam + res );
                    }
                } catch ( Exception e ) {
                    exce = new OGCWebServiceException_Impl( "RemoteWMS:handleGetMap", 
                                                            "Could not get map from RemoteWMS: " + 
                                                            capabilities.getService().getName() + 
                                                            "; " + "request: " + lparam + " " + 
                                                            e.toString() );
                }

                WMSGetMapResponse response = WMSProtocolFactory.createWMSGetMapResponse( lrequest, 
                                                                                         exce, 
                                                                                         result );
                OGCWebServiceEvent event = new OGCWebServiceEvent_Impl( this, response, "" );
                lclient.write( event );
            }
        }.start();

        Debug.debugMethodEnd();
    }

    /**
     * reads feature infos from the remote WMS by performing a FeatureInfo
     * request against it. As long the result of a FeatureInfo request is generic
     * (for usual it is som HTML) it isn't easy to combine the result with that
     * of other WMS's
     *
     * @param request feature info request
     * @return feaure info(s) decoded in the requested fromat
     */
    protected void handleFeatureInfo( WMSFeatureInfoRequest request, OGCWebServiceClient client )
                              throws WebServiceException {
        Debug.debugMethodBegin( this, "handleFeatureInfo" );

        URL url = null;

        if ( request.getVersion().equals( "1.0.0" ) ) {
            url = (URL)addresses.get( Operation.FEATUREINFO_NAME );
        } else {
            url = (URL)addresses.get( Operation.GETFEATUREINFO_NAME );
        }

        if ( url == null ) {
            throw new WebServiceException( "GetFeatureInfo is not supported by " + 
                                           "the RemoteWMS: " + 
                                           capabilities.getService().getName() );
        }

        String remoteAddress = NetWorker.url2String( url );
        String param = request.getRequestParameter();
        String us = remoteAddress + "?" + param;

        new RemoteWMSHandler( request, client, us, param ) {
            public void run() {
                OGCWebServiceException exce = null;
                String result = null;

                try {
                    URL ur = new URL( laddress );
                    // get map from the remote service                    
                    NetWorker nw = new NetWorker( ur );
                    byte[] b = nw.getDataAsByteArr( 20000 );
                    String contentType = nw.getContentType();

                    if ( contentType.equalsIgnoreCase( "application/vnd.ogc.gml" ) ) {
                        result = new String( b );
                    } else {
                        exce = new OGCWebServiceException_Impl( "RemoteWMS:handleFeatureInfo", 
                                                                "Response of the remote " + 
                                                                "WMS contains unknown content " + 
                                                                "type: " + contentType + 
                                                                ";request: " + lparam );
                    }
                } catch ( Exception e ) {
                    exce = new OGCWebServiceException_Impl( "RemoteWMS:handleFeatureInfo", 
                                                            "Could not get map from RemoteWMS: " + 
                                                            capabilities.getService().getName() + 
                                                            "; request: " + lparam + "; " + 
                                                            e.toString() );
                }

                WMSFeatureInfoResponse response = WMSProtocolFactory.createWMSFeatureInfoResponse( 
                                                          lrequest, exce, result );
                OGCWebServiceEvent event = new OGCWebServiceEvent_Impl( this, response, "" );
                lclient.write( event );
            }
        }.start();

        Debug.debugMethodEnd();
    }

    /**
     * reads the capabilities from the remote WMS by performing a GetCapabilities
     * request against it.
     *
     * @param request capabilities request
     */
    protected void handleGetCapabilities( WMSGetCapabilitiesRequest request, 
                                          OGCWebServiceClient client ) throws WebServiceException {
        Debug.debugMethodBegin( this, "handleGetCapabilities" );

        URL url = null;

        if ( request.getVersion().equals( "1.0.0" ) ) {
            url = (URL)addresses.get( Operation.CAPABILITIES_NAME );
        } else {
            url = (URL)addresses.get( Operation.GETCAPABILITIES_NAME );
        }

        String remoteAddress = NetWorker.url2String( url );
        String param = request.getRequestParameter();
        String us = remoteAddress + "?" + param;

        new RemoteWMSHandler( request, client, us, param ) {
            public void run() {
                OGCWebServiceException exce = null;
                WMSCapabilities result = null;

                try {
                    URL ur = new URL( laddress );
                    // get map from the remote service                    
                    NetWorker nw = new NetWorker( ur );
                    byte[] b = nw.getDataAsByteArr( 20000 );
                    String contentType = nw.getContentType();

                    if ( MimeTypeMapper.isKnownMimeType( contentType ) ) {
                        // create a WMSCapabilities instance from the result
                        StringReader reader = new StringReader( new String( b ) );
                        OGCWMSCapabilitiesFactory fac = new OGCWMSCapabilitiesFactory();
                        result = fac.createCapabilities( reader );
                    } else {
                        exce = new OGCWebServiceException_Impl( "RemoteWMS:handleGetCapabilities", 
                                                                "Response of the remote " + 
                                                                "WMS contains unknown content " + 
                                                                "type: " + contentType + 
                                                                ";request: " + lparam );
                    }
                } catch ( Exception e ) {
                    exce = new OGCWebServiceException_Impl( "RemoteWMS:handleGetCapabilities", 
                                                            "Could not get map from RemoteWMS: " + 
                                                            capabilities.getService().getName() + 
                                                            "; request: " + lparam + "; " + 
                                                            e.toString() );
                }

                WMSGetCapabilitiesResponse response = WMSProtocolFactory.createWMSGetCapabilitiesResponse( 
                                                              lrequest, exce, result );
                OGCWebServiceEvent event = new OGCWebServiceEvent_Impl( this, response, "" );
                lclient.write( event );
            }
        }.start();

        Debug.debugMethodEnd();
    }

    /**
     *
     *
     * @param request get styles request (WMS 1.1.1 - SLD)
     */
    protected void handleGetStyles( WMSGetStylesRequest request, OGCWebServiceClient client )
                            throws WebServiceException {
        Debug.debugMethodBegin( this, "handleGetStyles" );

        URL url = (URL)addresses.get( Operation.GETSTYLES_NAME );

        if ( url == null ) {
            throw new WebServiceException( "GetStyles is not supported by " + "the RemoteWMS: " + 
                                           capabilities.getService().getName() );
        }

        String remoteAddress = NetWorker.url2String( url );
        String param = request.getRequestParameter();
        String us = remoteAddress + "?" + param;

        // FIXME
        // TODO
        Debug.debugMethodEnd();
    }

    /**
     *
     *
     * @param request put styles request (WMS 1.1.1 - SLD)
     */
    protected void handlePutStyles( WMSPutStylesRequest request, OGCWebServiceClient client )
                            throws WebServiceException {
        Debug.debugMethodBegin( this, "handlePutStyles" );

        URL url = (URL)addresses.get( Operation.PUTSTYLES_NAME );

        if ( url == null ) {
            throw new WebServiceException( "PUTSTYLES is not supported by " + "the RemoteWMS: " + 
                                           capabilities.getService().getName() );
        }

        String remoteAddress = NetWorker.url2String( url );
        String param = request.getRequestParameter();
        String us = remoteAddress + "?" + param;

        // FIXME
        // TODO
        Debug.debugMethodEnd();
    }

    /**
     *
     *
     * @param request describe layer request (WMS 1.1.1 - SLD)
     */
    protected void handleDescribeLayer( WMSDescribeLayerRequest request, OGCWebServiceClient client )
                                throws WebServiceException {
        Debug.debugMethodBegin( this, "handleDescribeLayer" );

        URL url = (URL)addresses.get( Operation.DESCRIBELAYER_NAME );

        if ( url == null ) {
            throw new WebServiceException( "DESCRIBELAYER is not supported by " + 
                                           "the RemoteWMS: " + 
                                           capabilities.getService().getName() );
        }

        String remoteAddress = NetWorker.url2String( url );
        String param = request.getRequestParameter();
        String us = remoteAddress + "?" + param;

        // FIXME
        // TODO
        Debug.debugMethodEnd();
    }

    /**
     *
     *
     * @param request describe layer request (WMS 1.1.1 - SLD)
     */
    protected void handleGetLegendGraphic( WMSGetLegendGraphicRequest request, 
                                           OGCWebServiceClient client ) throws WebServiceException {
        Debug.debugMethodBegin( this, "handleGetLegendGraphic" );

        URL url = (URL)addresses.get( Operation.GETLEGENDGRAPHIC_NAME );

        if ( url == null ) {
            throw new WebServiceException( "GETLEGENDGRAPHIC is not supported by " + 
                                           "the RemoteWMS: " + 
                                           capabilities.getService().getName() );
        }

        String remoteAddress = NetWorker.url2String( url );
        String param = request.getRequestParameter();
        String us = remoteAddress + "?" + param;

        // FIXME
        // TODO
        Debug.debugMethodEnd();
    }

    /**
     * 
     *
     * @version $Revision$
     * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
     */
    protected abstract class RemoteWMSHandler extends Thread {
        protected OGCWebServiceClient lclient = null;
        protected OGCWebServiceRequest lrequest = null;
        protected String laddress = null;
        protected String lparam = null;

        /**
         * Creates a new RemoteWMSHandler object.
         *
         * @param request 
         * @param client 
         * @param address 
         * @param param 
         */
        RemoteWMSHandler( OGCWebServiceRequest request, OGCWebServiceClient client, String address, 
                          String param ) {
            this.lclient = client;
            this.laddress = address;
            this.lparam = param;
            this.lrequest = request;
        }

        /**
         *
         *
         * @param is 
         *
         * @return 
         *
         * @throws IOException 
         */
        protected String getInputStreamContent( InputStream is ) throws IOException {
            StringBuffer sb = new StringBuffer( 1000 );
            int c = 0;

            while ( ( c = is.read() ) >= 0 ) {
                sb.append( (char)c );
            }

            is.close();
            return sb.toString();
        }
    }
}