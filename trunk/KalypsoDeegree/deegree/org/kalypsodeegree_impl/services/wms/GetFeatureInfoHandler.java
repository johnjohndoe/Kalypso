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

import java.awt.Color;
import java.io.StringReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;

import org.deegree.gml.GMLFeature;
import org.deegree.gml.GMLFeatureCollection;
import org.deegree.gml.GMLProperty;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Position;
import org.deegree.services.OGCWebServiceClient;
import org.deegree.services.OGCWebServiceEvent;
import org.deegree.services.OGCWebServiceException;
import org.deegree.services.OGCWebServiceResponse;
import org.deegree.services.WebServiceException;
import org.deegree.services.wcs.protocol.WCSGetCoverageResponse;
import org.deegree.services.wfs.capabilities.WFSCapabilities;
import org.deegree.services.wfs.filterencoding.Filter;
import org.deegree.services.wfs.protocol.WFSGetFeatureRequest;
import org.deegree.services.wfs.protocol.WFSGetFeatureResponse;
import org.deegree.services.wfs.protocol.WFSQuery;
import org.deegree.services.wms.InvalidSRSException;
import org.deegree.services.wms.LayerNotDefinedException;
import org.deegree.services.wms.LayerNotQueryableException;
import org.deegree.services.wms.capabilities.DataSource;
import org.deegree.services.wms.capabilities.Layer;
import org.deegree.services.wms.capabilities.WMSCapabilities;
import org.deegree.services.wms.protocol.WMSFeatureInfoRequest;
import org.deegree.services.wms.protocol.WMSFeatureInfoResponse;
import org.deegree.services.wms.protocol.WMSGetMapRequest;
import org.deegree.services.wms.protocol.WMSGetMapResponse;
import org.deegree.xml.XMLParsingException;
import org.deegree.xml.XMLTools;
import org.deegree_impl.graphics.transformation.WorldToScreenTransform;
import org.deegree_impl.model.cs.ConvenienceCSFactory;
import org.deegree_impl.model.cs.CoordinateSystem;
import org.deegree_impl.model.ct.GeoTransformer;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.services.OGCWebServiceEvent_Impl;
import org.deegree_impl.services.OGCWebServiceException_Impl;
import org.deegree_impl.services.wfs.filterencoding.ComplexFilter;
import org.deegree_impl.services.wfs.filterencoding.FeatureFilter;
import org.deegree_impl.services.wfs.filterencoding.FeatureId;
import org.deegree_impl.services.wfs.protocol.WFSProtocolFactory;
import org.deegree_impl.services.wms.protocol.WMSGetMapRequest_Impl;
import org.deegree_impl.services.wms.protocol.WMSProtocolFactory;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.IDGenerator;
import org.deegree_impl.tools.NetWorker;
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Document;
import org.w3c.dom.Node;


/**
 * 
 *
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 */
class GetFeatureInfoHandler {
	protected WMSFeatureInfoRequest request = null;
    protected WMSGetMapRequest getMapRequest = null;
    private WMSCapabilities capabilities = null;
    // collects the reponse for each layer
    private Object[] gml = null;        
    // scale of the map
    protected double scale = 0;
    // holds the number of request services that have responsed
    private int count = 0;
    // CRS of the request
    protected CS_CoordinateSystem reqCRS = null;       

    /**
     * Creates a new GetMapHandler object.        
     * @param GetMap request to perform
     */
    public GetFeatureInfoHandler(WMSCapabilities capabilities, WMSFeatureInfoRequest request) throws WebServiceException {
        this.request = request;         
        this.capabilities = capabilities;
        getMapRequest = request.getGetMapRequestCopy();
        try {
            reqCRS = createRequestCRS();
            if (reqCRS == null ) {
                throw new InvalidSRSException( "SRS: " + getMapRequest.getSrs() +
                                           "is nor known by the deegree WMS " );
            }
        } catch(Exception e) {
            throw new InvalidSRSException( "SRS: " + getMapRequest.getSrs() +
                                           "is nor known by the deegree WMS " );
        }
        try {
            scale = calcScale();
        } catch(Exception e) {
            Debug.debugException( e, " - " );
            throw new WebServiceException( "Couldn't calculate scale! " + e );
        }
        
    }
    
    /**
     * calculates the map scale as defined in the OGC WMS 1.1.1 specifications
     *
     * @return scale of the map
     */
    private double calcScale() throws Exception {

        GM_Envelope bbox = getMapRequest.getBoundingBox();              

        if ( !getMapRequest.getSrs().equalsIgnoreCase( "EPSG:4326" ) ) {
            // transform the bounding box of the request to EPSG:4326
            GeoTransformer transformer = new GeoTransformer( "EPSG:4326" );
            bbox = transformer.transformEnvelope( bbox, reqCRS ); 
        }        

        double dx = bbox.getWidth()/getMapRequest.getWidth();
        double dy = bbox.getHeight()/getMapRequest.getHeight();
        
        // create a box on the central map pixel to determine its size in meter
        GM_Position min = GeometryFactory.createGM_Position( bbox.getMin().getX() + 
                                      dx*(getMapRequest.getWidth()/2d-1), 
                                      bbox.getMin().getY() + 
                                      dy*(getMapRequest.getHeight()/2d-1) );
        GM_Position max = GeometryFactory.createGM_Position( bbox.getMin().getX() + 
                                      dx*(getMapRequest.getWidth()/2d), 
                                      bbox.getMin().getY() + 
                                      dy*(getMapRequest.getHeight()/2d) );

        return calcDistance( min.getY(), min.getX(), max.getY(), max.getX() );
    }
    
    /**
     * calculates the distance between two points in EPSG:4326 coodinates.
     */
    private double calcDistance(double lon1, double lat1, double  lon2, double lat2) {
        double  r   = 6368.137;
        double  rad = Math.PI/180d;
        double  cose = 0;

        cose = Math.sin(rad*lon1) * Math.sin(rad*lon2) + Math.cos(rad*lon1) * 
               Math.cos(rad*lon2) * Math.cos(rad*(lat1-lat2)) ;
        double dist= r * Math.acos(cose);

        return dist*1000;
    }
    
    /**
     * creates an object that represents the CRS of the GetMap request
     */
    private CS_CoordinateSystem createRequestCRS() {
        ConvenienceCSFactory cf = ConvenienceCSFactory.getInstance();
        CoordinateSystem csSource = cf.getCSByName( getMapRequest.getSrs() );
        if ( csSource == null ) return null;
        org.deegree_impl.model.cs.Adapters adapter = org.deegree_impl.model.cs.Adapters.getDefault();
        return adapter.export( csSource );
    }
    
    /**
     * increases the counter variable that holds the number of services that
     * has sent a response. All data are available if the counter value equals
     * the number of requested layers.
     */
    protected synchronized void increaseCounter() {
        count++;
    }        

    /**
     * performs a GetFeatureInfo request and retruns the result encapsulated within
     * a <tt>WMSFeatureInfoResponse</tt> object. <p>
     * The method throws an WebServiceException that only shall be thrown if an
     * fatal error occurs that makes it imposible to return a result.
     * If something wents wrong performing the request (none fatal error) The 
     * exception shall be encapsulated within the response object to be returned 
     * to the client as requested (GetFeatureInfo-Request EXCEPTION-Parameter).
     *     
     * @return response to the GetFeatureInfo response
     */
    public WMSFeatureInfoResponse performGetFeatureInfo() throws WebServiceException {
        Debug.debugMethodBegin( this, "performGetMap" );                
        
        String[] qlayers = request.getQueryLayers();
        
        // there must be one theme for each requested layer
        gml = new Object[ qlayers.length ];
        
        Layer[] layerList = new Layer[ qlayers.length ];
        for ( int i = 0; i < qlayers.length; i++ ) {
            layerList[i] = capabilities.getCapability().getLayer( qlayers[i] );      
            if ( layerList[i] == null ) {
                throw new LayerNotDefinedException( "Layer: " +qlayers[i] + " is not "  +
                                                    "known by the WMS" );
            }
            if ( !layerList[i].isQueryable() ) {
                throw new LayerNotQueryableException( "Layer: " + qlayers[i] + " is not "  +
                                                      "queryable" );
            }
            if ( !layerList[i].isSrsSupported( getMapRequest.getSrs() ) ) {
                 throw new InvalidSRSException( "SRS: " + getMapRequest.getSrs() +
                                                "is not known by layer: " + qlayers[i] );
            }
        }

        // invokes the data supplyer for each layer in an independ thread
        for ( int i = 0; i < layerList.length; i++ ) {   

            if ( validate( layerList[i], qlayers[i] ) ) {                 
                ServiceInvoker si = new ServiceInvoker( layerList[i], i );
                si.start();
            } else {
                // set theme to null if no data are available for the requested
                // area and/or scale
                gml[ i ] = null;
                increaseCounter();
            }
        }

        // waits until the requested layers are available as <tt>DisplayElements</tt>
        // or the time limit has been reached.
        try {
            waitForFinish();
        } catch ( WebServiceException we ) {
            Debug.debugException( we, " - " );
            return createExceptionResponse( we );
        } catch ( Exception e ) {
            Debug.debugException( e, " - " );
            return createExceptionResponse( e );
        }

        WMSFeatureInfoResponse res = createFeatureInfoResponse();        

        Debug.debugMethodEnd();

        return res;
    }
    
    /**
     * validates if the requested layer matches the conditions of the request
     * if not a <tt>WebServiceException</tt> will be thrown. If the layer matches
     * the request, but isn't able to deviever data for the requested area and/or
     * scale false will be returned. If the layer matches the request and contains
     * data for the requested area and/or scale true will be returned.
     * 
     * @param layer layer as defined at the capabilities/configuration
     * @param name name of the layer (must be submitted seperatly because the
     *             layer parameter can be <tt>null</tt>
     */
    private boolean validate(Layer layer, String name) throws WebServiceException {
        
        Debug.debugMethodBegin( this, "validate" );
                
        // check for valid coordinated reference system
        String[] srs = layer.getSrs();
        boolean tmp = false;
        for (int i = 0; i < srs.length; i++) {
            if ( srs[i].equalsIgnoreCase( getMapRequest.getSrs() ) ) {
                tmp = true;
                break;
            }
        }
   
        if ( !tmp ) {
            throw new InvalidSRSException( "layer: " + name + " can't be " +
                                           "delievered in SRS: " + getMapRequest.getSrs() );
        }
        
        // check scale
        if ( layer.getDataSource( scale ) == null ) {
            return false;
        }

        // check bounding box
        try {
            
            GM_Envelope bbox = getMapRequest.getBoundingBox();
            GM_Envelope layerBbox = layer.getLatLonBoundingBox();
            if ( !getMapRequest.getSrs().equalsIgnoreCase( "EPSG:4326" ) ) {
                // transform the bounding box of the request to EPSG:4326
                GeoTransformer transformer = new GeoTransformer( "EPSG:4326" );
                bbox = transformer.transformEnvelope( bbox, reqCRS );
            }
            
            if ( !bbox.intersects( layerBbox ) ) {
                return false;
            }
            
        } catch(Exception e) {
            Debug.debugException( e, " - " );
            throw new WebServiceException( "couldn't compare bounding boxes\n" + 
                                           e.toString() );
        }
  
        Debug.debugMethodEnd();
        
        return true;
    }
    

    /**
     * creates a <tt>WMSGetMapResponse</tt> containing an <tt>OGCWebServiceException</tt>
     *
     * @param e exception to encapsulate into the response
     */
    private WMSFeatureInfoResponse createExceptionResponse( Exception e ) {
        
        OGCWebServiceException exce = null;     
        
        // default --> application/vnd.ogc.se_xml
        exce = 
            new OGCWebServiceException_Impl( "GetFeatureInfoHandler", e.getMessage() );
       
        WMSFeatureInfoResponse res = 
            WMSProtocolFactory.createWMSFeatureInfoResponse(request, exce, null );
        
        return res;
    }

    /**
     * waits until the requested layers are available as <tt>DisplayElements</tt>
     * or the time limit has been reached. If the waiting is terminated by reaching
     * the time limit an <tt>WebServiceException</tt> will be thrown to indicated
     * that the request couldn't be performed correctly.
     *
     * @throws WebServiceException if the time limit has been reached
     */
    private void waitForFinish() throws WebServiceException, Exception {
        Debug.debugMethodBegin( this, "waitForFinish" );

        // subtract 1 second for architecture overhead and image creation
        long timeLimit = 1000 * (capabilities.getDeegreeParam().getRequestTimeLimit()-1);
        //long timeLimit = 1000 * 100;
        long runTime = 0;

        while ( count < gml.length ) {
            try {
                Thread.sleep( 100 );
            } catch ( Exception e ) {
                Debug.debugException( e, " - " );
                throw new Exception( "Exception in handling Thread in waiting loop" + "\n" + e );
            }

            runTime += 100;

            // finish loop after if request performing hasn't been completed 
            // after the time limit is reached
            if ( runTime > timeLimit ) {
                throw new WebServiceException( "GetMap request performing exceeds "+
                                               "timelimit! " );
            }
        }

        Debug.debugMethodEnd();
    }
    
    /**
     * put a GML document to the passed index of the gml array. The second param
     * passed is a GML document or an exception
     */
    protected void putGML(int index, Object o ) {
        gml[ index ] = o;
    }

    /**
     * generates the desired output from the GMLs
     */
    private WMSFeatureInfoResponse createFeatureInfoResponse()  {
        Debug.debugMethodBegin( this, "createFeatureInfoResponse" );
        
        GM_Envelope bbox = getMapRequest.getBoundingBox();
        
        StringBuffer sb = new StringBuffer( 20000 );        
        sb.append( "<ll:FeatureCollection xmlns:gml='http://www.opengis.net/gml' " );
        sb.append( "xmlns:ll='http://www.lat-lon.de' " );
        sb.append( "xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' " );
        URL url = capabilities.getDeegreeParam().getSchemaLocation();
        if ( url != null ) {
            sb.append( "xsi:schemaLocation='" );        
            sb.append( "http://www.lat-lon.de " + NetWorker.url2String( url ) + "'" );
        }
        sb.append( "><gml:boundedBy>");
        sb.append("<gml:Box srsName='" + getMapRequest.getSrs() + "'>" );
        sb.append("<gml:coordinates>" + bbox.getMin().getX() + "," );
        sb.append(bbox.getMin().getY() + " " + bbox.getMax().getX() + "," );
        sb.append(bbox.getMax().getY() + "</gml:coordinates >" ) ;
        sb.append("</gml:Box></gml:boundedBy>" );
        
        int cnt = 0;

        for (int i = 0; i < gml.length; i++) {
            GMLFeatureCollection gfc = (GMLFeatureCollection)gml[i];
            GMLFeature[] feat = gfc.getFeatures();
            if ( feat != null ) {
                for (int j = 0; j < feat.length; j++) {                
                    GMLProperty[] props = feat[j].getNoneGeoProperties();
                    if ( props != null ) {
                        cnt++;
                        sb.append( "<gml:featureMember>" );                        
                        sb.append( "<ll:" + feat[j].getFeatureTypeName() );                        
                        sb.append( " fid='" + feat[j].getId().replace(' ','_') + "'>" );
                        for (int k = 0; k < props.length; k++) {
                            sb.append( "<ll:" +  props[k].getName() + ">" );
                            sb.append( props[k].getPropertyValue() );
                            sb.append( "</ll:" +  props[k].getName() + ">" );
                        }
                        sb.append( "</ll:" + feat[j].getFeatureTypeName() +">" );
                        sb.append( "</gml:featureMember>" );
                        if ( cnt >= request.getFeatureCount() ) break;
                    }
                }                           
            }
            if ( cnt >= request.getFeatureCount() ) break;
        }
        sb.append( "</ll:FeatureCollection>" );

        WMSFeatureInfoResponse response = 
            WMSProtocolFactory.createWMSFeatureInfoResponse( request, null, sb.toString() );
     
        Debug.debugMethodEnd();
        return response;
    }
    
    ////////////////////////////////////////////////////////////////////////////
    //                          inner classes                                 //
    ////////////////////////////////////////////////////////////////////////////

    /**
     * Inner class for accessing the data of one layer and creating 
     * a GML document from it. The class extends <tt>Thread</tt> and implements 
     * the run method, so that a parallel data accessing from several layers is 
     * possible.
     *
     * @version $Revision$
     * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
     */
    private class ServiceInvoker extends Thread implements OGCWebServiceClient {
        private Layer layer = null;
        private int index = 0;

        /**
         * Creates a new ServiceInvoker object.
         *
         * @param layer 
         * @param index index of the requested layer
         */
        ServiceInvoker( Layer layer, int index ) {
            this.layer = layer;
            this.index = index;
        }

        /**
         * overrides the run-method of the parent class <tt>Thread</tt> for 
         * enabling a multi-threaded access to the data.
         */
        public void run() {
            Debug.debugMethodBegin( this, "run" );
            
            DataSource ds = layer.getDataSource( scale );
            
            if ( ds != null ) {
                OGCWebServiceEvent event = null;
                try {                    
                    int type = ds.getType();                                    
                    switch ( type ) {
                        case DataSource.LOCALWFS: 
                        case DataSource.REMOTEWFS: {
                            event = createGetFeatureRequest( ds );
                            break;
                        }
                        case DataSource.LOCALWCS: 
                        case DataSource.REMOTEWCS: {
                            event = createDescribeCoverageRequest( ds );
                            break;
                        }
                        case DataSource.REMOTEWMS: {
                            event = createGetFeatureInfo( ds );
                            break;
                        }
                    }
                } catch(Exception e) {
                    Debug.debugException( e, " - " );
                    OGCWebServiceException exce = 
                        new OGCWebServiceException_Impl( "ServiceInvoker: " + layer.getName(),                                                     
                                                         "Couldn't create query!" + 
                                                         e.toString());
                    putGML( index, exce );
                    increaseCounter();
                    Debug.debugMethodEnd();
                    return;
                }
                
                try {
                    ds.getOGCWebService().doService( event );
                } catch(Exception e) {
                    Debug.debugException( e, " - " );
                    OGCWebServiceException exce = 
                        new OGCWebServiceException_Impl( "ServiceInvoker: " + layer.getName(),                                                     
                                                         "Couldn't perform doService()!" +
                                                         e.toString());
                    putGML( index, exce );
                    increaseCounter();
                    Debug.debugMethodEnd();
                    return;
                }
                
            } else {
                // increase counter because there is no service to call so it
                // is assumed that the request for the current layer if fullfilled
                increaseCounter();
            }
            
            Debug.debugMethodEnd();
        }
        
        /**
         * creates a getFeature request considering the getMap request and the
         * filterconditions defined in the submitted <tt>DataSource</tt> object.
         * The request will be encapsualted within a <tt>OGCWebServiceEvent</tt>.
         * @return envent object containing a GetFeature request
         */
        private OGCWebServiceEvent createGetFeatureRequest(DataSource ds) throws Exception {
            Debug.debugMethodBegin( this, "createGetFeatureRequest" );            
            
            GM_Envelope targetArea = calcTargetArea( ds );
            
            // no filter condition has been defined
            StringBuffer sb = new StringBuffer( 2000 );
            sb.append( "<?xml version='1.0' encoding='UTF-8'?>" );
            sb.append( "<GetFeature xmlns='http://www.opengis.net/wfs' " );
            sb.append( "xmlns:ogc='http://www.opengis.net/ogc' ");            
            sb.append( "xmlns:gml='http://www.opengis.net/gml' " );                
            sb.append( "service='WFS' version='1.0.0' " );                     
            sb.append( "outputFormat='GML2'>");            
            sb.append( "<Query typeName='" + ds.getName() + "'><ogc:Filter>" );
            
            WFSQuery query = ds.getQuery();
            if ( query == null ) {  
//                sb.append("<ogc:And>" );
                // BBOX operation for speeding up the search at simple datasources
                // like shapes
                sb.append("<ogc:BBOX><PropertyName>" + ds.getGeometryProperty() );
                sb.append("</PropertyName>" );
                sb.append("<gml:Box srsName='" + getMapRequest.getSrs() + "'>" );
                sb.append("<gml:coordinates>" + targetArea.getMin().getX() + "," );
                sb.append(targetArea.getMin().getY() + " " + targetArea.getMax().getX() + "," );
                sb.append(targetArea.getMax().getY() + "</gml:coordinates >" ) ;
                sb.append("</gml:Box></ogc:BBOX>" );
               
                // Intersects for exact searching
//                sb.append("<ogc:Intersects>");
//				sb.append("<PropertyName>" ).append( ds.getGeometryProperty() );
//				sb.append("</PropertyName>");
//                GM_Object geom = GeometryFactory.createGM_Surface ( targetArea,  null );
//                sb.append( GMLAdapter.export( geom ) );
//                sb.append("</ogc:Intersects>");
//                sb.append("</ogc:And>" );
                sb.append("</ogc:Filter></Query></GetFeature>" );
            } else {
                Filter filter = query.getFilter();
                                                                                
                sb.append( "<ogc:And>" );
                // BBOX operation for speeding up the search at simple datasources
                // like shapes
                sb.append("<ogc:BBOX><PropertyName>" + ds.getGeometryProperty() );
                sb.append("</PropertyName><gml:Box>" );
                sb.append("<gml:coordinates>" + targetArea.getMin().getX() + "," );
                sb.append(targetArea.getMin().getY() + " " + targetArea.getMax().getX() + "," );
                sb.append(targetArea.getMax().getY() + "</gml:coordinates >" ) ;
                sb.append("</gml:Box></ogc:BBOX>" );
                
                // Intersects for exact searching
//                sb.append("<ogc:Intersects>");
//				sb.append("<PropertyName>" ).append( ds.getGeometryProperty() );
//				sb.append("</PropertyName>");
//                GM_Object geom = GeometryFactory.createGM_Surface ( targetArea,  null );
//                sb.append( GMLAdapter.export( geom ) );
//                sb.append("</ogc:Intersects>");
                
                if ( filter instanceof ComplexFilter ) {
                    org.deegree.services.wfs.filterencoding.Operation op = ((ComplexFilter)filter).getOperation();
                    sb.append( op.toXML() );
                } else {
                    ArrayList featureIds = ((FeatureFilter)filter).getFeatureIds();
                    for (int i = 0; i < featureIds.size (); i++) {
                        FeatureId fid = (FeatureId) featureIds.get (i);
                        sb.append (fid.toXML () );
                    }
                }
                
                sb.append("</ogc:And></ogc:Filter></Query></GetFeature>" );
                
            }

            // create dom representation of the request
            StringReader sr = new StringReader( sb.toString() );
            Document doc = XMLTools.parse( sr );
            
            // create OGCWebServiceEvent object
            IDGenerator idg = IDGenerator.getInstance ();
            WFSGetFeatureRequest gfr = 
            	WFSProtocolFactory.createWFSGetFeatureRequest( ""+ idg.generateUniqueID (), doc );

            OGCWebServiceEvent event = new OGCWebServiceEvent_Impl( this, gfr, "", this );

            Debug.debugMethodEnd();
            return event;            
        }
        
        /**
         * calculates the target area for the getfeatureinfo request from the
         * maps bounding box, the its size and the image coordinates of interest. An
         * area is calculated instead of using a point because to consider 
         * uncertainties determining the point of interest
         * @param ds <tt>DataSource</tt> of the layer that is requested for feature 
         *          infos (each layer may be offered in its own crs)
         */
        private GM_Envelope calcTargetArea(DataSource ds) throws WebServiceException {
            Debug.debugMethodBegin( this, "calcTargetPoint" );      
            
            int width = request.getGetMapRequestCopy().getWidth();
            int height = request.getGetMapRequestCopy().getHeight();
            int x = request.getClickPoint().x;
            int y = request.getClickPoint().y ;            
            
            GM_Envelope bbox = request.getGetMapRequestCopy().getBoundingBox();
            
            // transform request bounding box to the coordinate reference
            // system the WFS holds the data if requesting CRS and WFS-Data
            // crs are different
            WFSCapabilities capa = (WFSCapabilities)ds.getOGCWebService().getCapabilities();

            org.deegree.services.wfs.capabilities.FeatureType ft = 
                        capa.getFeatureTypeList().getFeatureType( ds.getName() );

            if ( ft == null ) {
                throw new WebServiceException( "Feature Type: " + ds.getName() +    
                                               " is not known by the WFS" );
            }
            String crs = ft.getSrs(); 
            GM_Envelope tBbox = null;
            try {
                if ( !(crs.equalsIgnoreCase( request.getGetMapRequestCopy().getSrs() )) ) {
                    GeoTransformer transformer = new GeoTransformer( crs );
                    bbox = transformer.transformEnvelope( bbox, reqCRS );
                }
            
                GeoTransform gt = 
                    new WorldToScreenTransform( bbox.getMin().getX(),  bbox.getMin().getY(),
                                                bbox.getMax().getX(),  bbox.getMax().getY(),
                                                0, 0, width-1, height-1 );
                double[] target = new double[4];
                // 10-pixel area
                target[0] = gt.getSourceX( x-5 );
                target[1] = gt.getSourceY( y+5 );
                target[2] = gt.getSourceX( x+5 );
                target[3] = gt.getSourceY( y-5 );

                tBbox = GeometryFactory.createGM_Envelope( target[0], target[1], 
                										   target[2], target[3] );
            } catch(Exception e) {
                throw new WebServiceException( e.toString() );
            }

            Debug.debugMethodEnd();
            return tBbox;
        }
        
        /**
         * creates a describe Coverage request
         * The request will be encapsualted within a <tt>OGCWebServiceEvent</tt>.
         * @return envent object containing a DescribeCoverage request
         */
        private OGCWebServiceEvent createDescribeCoverageRequest(DataSource ds) {
            Debug.debugMethodBegin();

            OGCWebServiceEvent event = new OGCWebServiceEvent_Impl( this, null, "", this );

            Debug.debugMethodEnd();
            return event;
        }
        
        /**
         * creates a GetFeatureInfo request for requesting a cascaded remote WMS          
         * The request will be encapsualted within a <tt>OGCWebServiceEvent</tt>.
         * @return envent object containing a GetFeatureInfo request
         */
        private OGCWebServiceEvent createGetFeatureInfo(DataSource ds) throws XMLParsingException {
            Debug.debugMethodBegin(  );
            
            // create embbeded map request
            WMSGetMapRequest gmr = ds.getGetMapRequest();    
            
            String format = getMapRequest.getFormat();

            if (  gmr != null && !"%default%".equals( gmr.getFormat() ) ) {
                format = gmr.getFormat();
            }

            org.deegree.services.wms.protocol.WMSGetMapRequest.Layer[] lys = null;
            lys = new org.deegree.services.wms.protocol.WMSGetMapRequest.Layer[1];
            lys[0] = WMSGetMapRequest_Impl.createLayer( layer.getName(), "default" );

            if (  gmr != null && gmr.getLayers() != null ) {                
                lys = gmr.getLayers();
            }
            Color bgColor = getMapRequest.getBGColor();
            if (  gmr != null && gmr.getBGColor() != null ) {
                bgColor = gmr.getBGColor();
            }
            String time = getMapRequest.getTime();
            if (  gmr != null && gmr.getTime() != null ) {
                time = gmr.getTime() ;
            }
            HashMap vendorSpecificParameter = getMapRequest.getVendorSpecificParameters();
            if ( gmr != null && gmr.getVendorSpecificParameters() != null &&
                 gmr.getVendorSpecificParameters().size() > 0 ) {
                vendorSpecificParameter = gmr.getVendorSpecificParameters();
            }
            String version = "1.1.0";
            if ( gmr != null && gmr.getVersion() != null ) {
                version = gmr.getFormat();
            }
            double[] elevation = getMapRequest.getElevation();
            if (  gmr != null && gmr.getElevation() != null ) {
                elevation = gmr.getElevation();
            }
            String[] sampleDim = null;
            if (  gmr != null && gmr.getSampleDimension() != null ) {
                sampleDim = gmr.getSampleDimension();
            }
            
            IDGenerator idg = IDGenerator.getInstance ();
            gmr = WMSProtocolFactory.createGetMapRequest( version, 
                                                          ""+idg.generateUniqueID (), 
                                                          lys, elevation, 
                                                          sampleDim, 
                                                          format, getMapRequest.getWidth(), 
                                                          getMapRequest.getHeight(), 
                                                          getMapRequest.getSrs(), 
                                                          getMapRequest.getBoundingBox(), 
                                                          getMapRequest.getTransparency(), 
                                                          bgColor, 
                                                          getMapRequest.getExceptions(), 
                                                          time, null, null, 
                                                          vendorSpecificParameter );
            
            // create GetFeatureInfo request for cascaded/remote WMS
            String[] queryLayers = new String[] { layer.getName() };
            WMSFeatureInfoRequest req = 
                WMSProtocolFactory.createGetFeatureInfoRequest( "1.1.0", this.toString(), 
                                                                queryLayers, gmr,
                                                                request.getInfoFormat(), 
                                                                request.getFeatureCount(), 
                                                                request.getClickPoint(), 
                                                                request.getExceptions(), 
                                                                null, request.getVendorSpecificParameters() );
            
            OGCWebServiceEvent event = new OGCWebServiceEvent_Impl( this, req, "", this );
            
            Debug.debugMethodEnd();
            return event;            
        }

        /**
         * The method implements the <tt>OGCWebServiceClient</tt> interface. So a
         * deegree OWS implementation accessed by this class is able to return the
         * result of a request by calling the write-method.
         * @param response to a GetXXX request
         */
        public void write( Object result ) {                        

            try {

                OGCWebServiceEvent event = (OGCWebServiceEvent)result;          
                OGCWebServiceResponse res = event.getResponse();
                if ( res.getException() != null ) {
                    Node message = res.getException().getElementsByTagName( "message" ).item(0);
                    Node location = res.getException().getElementsByTagName( "locator" ).item(0);
                    OGCWebServiceException exce = 
                        new OGCWebServiceException_Impl( "ServiceInvoker: " + layer.getName() + "\n" +
                                                         location.getFirstChild().getNodeValue(), 
                                                         message.getFirstChild().getNodeValue());
                    putGML(index, exce );
                } else {
                    if ( res instanceof WMSGetMapResponse ) {
                        handleGetFeatureInfoResponse( (WMSGetMapResponse)res );
                    } else if ( res instanceof WFSGetFeatureResponse ) {
                        handleGetFeatureResponse( (WFSGetFeatureResponse)res );
                    } else if ( res instanceof WCSGetCoverageResponse ) {
                        handleDescribeCoverageResponse( (WCSGetCoverageResponse) res);
                    } else {
                        OGCWebServiceException exce = 
                            new OGCWebServiceException_Impl( "ServiceInvoker: " + layer.getName(),                                                     
                                                             "unknown response format!");
                        putGML( index, exce );
                    }
                }
            } catch(Exception e) {
                Debug.debugException( e, " - " );
                OGCWebServiceException exce = 
                    new OGCWebServiceException_Impl( "ServiceInvoker: " + layer.getName(),                                                     
                                                     e.toString() );
                putGML( index, exce );
            }
            
            // increase counter to indicate that one more layers requesting is 
            // completed
            increaseCounter();
            
        }
        
        /**
         * handles the response of a cascaded WMS and calls a factory to create
         * <tt>DisplayElement</tt> and a <tt>Theme</tt> from it
         */
        private void handleGetFeatureInfoResponse(WMSGetMapResponse response) throws Exception {
            Debug.debugMethodBegin(  );
                        
            // TODO
            putGML( index, null );
            
            Debug.debugMethodEnd();
        }
        
        /**
         * handles the response of a WFS and calls a factory to create
         * <tt>DisplayElement</tt> and a <tt>Theme</tt> from it
         */
        private void handleGetFeatureResponse(WFSGetFeatureResponse response) 
                                              throws Exception {
            Debug.debugMethodBegin( this, "handleGetFeatureResponse" );
            
            GMLFeatureCollection gfc = null;
            
            Object o = response.getResponse();
            if ( o instanceof GMLFeatureCollection ) {
                gfc = (GMLFeatureCollection)o ;
            } else {
                throw new Exception( "unknown data format at a GetFeature response" );
            }
            putGML( index, gfc );
            Debug.debugMethodEnd();
        }
        
        /**
         * handles the response of a WCS and calls a factory to create
         * <tt>DisplayElement</tt> and a <tt>Theme</tt> from it
         */
        private void handleDescribeCoverageResponse(WCSGetCoverageResponse response) 
                                               throws Exception {
            Debug.debugMethodBegin( this, "handleGetCoverageResponse" );
            
            //TODO
            putGML( index, null );
            
            Debug.debugMethodEnd();
        }
        
    }
           
}