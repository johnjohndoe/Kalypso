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
package org.deegree_impl.services.wcas;

import java.io.InputStreamReader;
import java.net.URL;
import java.util.ArrayList;

import org.deegree.gml.GMLFeature;
import org.deegree.services.Handler;
import org.deegree.services.OGCWebServiceClient;
import org.deegree.services.OGCWebServiceEvent;
import org.deegree.services.OGCWebServiceException;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.OGCWebServiceResponse;
import org.deegree.services.WebServiceException;
import org.deegree.services.wcas.capabilities.FederatedCatalog;
import org.deegree.services.wcas.capabilities.WCASCapabilities;
import org.deegree.services.wcas.protocol.CASDelete;
import org.deegree.services.wcas.protocol.CASDescribeRecordTypeRequest;
import org.deegree.services.wcas.protocol.CASGetCapabilitiesRequest;
import org.deegree.services.wcas.protocol.CASGetRecordRequest;
import org.deegree.services.wcas.protocol.CASInsert;
import org.deegree.services.wcas.protocol.CASInsertResult;
import org.deegree.services.wcas.protocol.CASOperation;
import org.deegree.services.wcas.protocol.CASQuery;
import org.deegree.services.wcas.protocol.CASRegisterServiceRequest;
import org.deegree.services.wcas.protocol.CASTransactionRequest;
import org.deegree.services.wcas.protocol.CASUpdate;
import org.deegree.services.wfs.filterencoding.Filter;
import org.deegree.services.wfs.protocol.WFSDescribeFeatureTypeResponse;
import org.deegree.services.wfs.protocol.WFSGetFeatureRequest;
import org.deegree.services.wfs.protocol.WFSGetFeatureResponse;
import org.deegree.services.wfs.protocol.WFSInsertResult;
import org.deegree.services.wfs.protocol.WFSOperation;
import org.deegree.services.wfs.protocol.WFSQuery;
import org.deegree.services.wfs.protocol.WFSTransactionResponse;
import org.deegree.xml.DOMPrinter;
import org.deegree.xml.XMLTools;
import org.deegree_impl.services.OGCWebServiceEvent_Impl;
import org.deegree_impl.services.OGCWebServiceException_Impl;
import org.deegree_impl.services.OGCWebService_Impl;
import org.deegree_impl.services.wcas.protocol.CASProtocolFactory;
import org.deegree_impl.services.wfs.protocol.WFSProtocolFactory;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.NetWorker;
import org.deegree_impl.tools.TimeTools;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;


/**
 *
 * A WCAS implementns the <tt>WCASService</tt> interface to act like a OGC
 * web service. This means that a WCAS is callable
 * through the <tt>doService</tt>-method defined at <tt>OGCWebService</tt>.
 * <p></p>
 * Because the <tt>WCASService_Impl</tt> acts as proxy he also implements
 * the OGCWebServiceClient interface to receive the response(s) from
 * the WFS's he had called. So the WCAS is nothing else than a client
 * to a WFS.
 *
 * <p>-----------------------------------------------------</p>
 *
 * @author Andreas Poth
 * @version $Revision$ $Date$
 * <p>
 */
public class WCASService_Impl extends OGCWebService_Impl implements Handler {
    private ArrayList results = null;
    private Handler dispatcher = null;
    private OGCWebServiceClient dest = null;
    private OGCWebServiceRequest inRequest = null;
    private WCASCapabilities capa = null;
    private OGCWebServiceRequest[] requests = null;
    private int expectedResponses = 1;
    private int reqType = 0;

    /** constructor
     * @param capa catalog capabilities
     * catalog
     */
    public WCASService_Impl( WCASCapabilities capa ) {
        this.capa = capa;
        results = new ArrayList();
    }

    /** implements the <tt>doService</tt> method inherited from the
     * <tt>OGCWebService</tt> interface. The method receives a WCAS...
     * request delievers it to one or more WFS's
     * @param event event object that contains the requets to be performed
     */
    public void doService( OGCWebServiceEvent event ) throws WebServiceException {
        Debug.debugMethodBegin();

        // get destination to deliver the response too
        dest = event.getDestination();

        // get request to perform
        inRequest = event.getRequest();
        
        try {
            OGCWebServiceEvent[] wfsEvent = transformToWFSRequest( inRequest );

            // because each WFS have to answer to fullfill the requests
            // the number of expected responses equals the number of events
            expectedResponses = wfsEvent.length;

            requests = new OGCWebServiceRequest[wfsEvent.length];

            for ( int i = 0; i < wfsEvent.length; i++ ) {
                try {
                    requests[i] = wfsEvent[i].getRequest();
                    handleRequest( wfsEvent[i] );
                } catch ( Exception ex ) {
                    System.out.println( ex );
                }
            }
        } catch ( Exception ex ) {
        	ex.printStackTrace ();
            // TODO
            // create an exception response and call the write-method
            // of the CAS client
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
    public OGCWebServiceResponse doService( OGCWebServiceRequest request )
                                    throws WebServiceException {
        Debug.debugMethodBegin();
        Debug.debugMethodEnd();
        throw new NoSuchMethodError( "doService(OGCWebServiceRequest)" );
    }

    /**
     * handles the creation and send of a getRecord response back
     * to the client
     */
    private void handleGetRecordResponse() throws Exception {
        Debug.debugMethodBegin();

        int cntAll = 0;

        StringBuffer allSB = new StringBuffer( 100000 );

        for ( int i = 0; i < results.size(); i++ ) {
            if ( results.get( i ) instanceof WFSGetFeatureResponse ) {
                Document doc = (Document)( (WFSGetFeatureResponse)results.get( i ) ).getResponse();
                WFSGetFeatureRequest req = (WFSGetFeatureRequest)( (WFSGetFeatureResponse)results.get( i ) ).getRequest();
                String propertySetName = req.getHandle();

                int count = 0;
                // get service metadata
                NodeList nl = doc.getElementsByTagName( "ISO19119" );

                if ( ( nl == null ) || ( nl.getLength() == 0 ) ) {
                    // if no service metadata contained get data metadata
                    nl = doc.getElementsByTagName( "MD_Metadata" );
                }

                if ( ( nl == null ) || ( nl.getLength() == 0 ) ) {
                    // if no data metadata contained get hits
                    nl = doc.getElementsByTagName( "_COUNT_" );

                    if ( ( nl != null ) && ( nl.getLength() > 0 ) ) {
                        String tmp = nl.item( 0 ).getFirstChild().getNodeValue();
                        count = Integer.parseInt( tmp );
                    } // else if nor service, data or hits are contained then
                      // metadata sets have been found
                }

                if ( count == 0 ) {
                    count = nl.getLength();
                }

                cntAll += count;

                StringBuffer sb = new StringBuffer( 50000 );
                String time = TimeTools.getISOFormattedTime();
                sb.append( "<searchResult elementSetName=\"" + propertySetName + "\" " );
                sb.append( "success=\"true\" numberOfRecords=\"" + count + "\" " );
                sb.append( "schema=\"TC211\" timestamp=\"" + time + "\">" );

                // just required if the propertySetName equals Full, Brief
                // or Summary                
                if ( propertySetName.equals( "Full" ) || propertySetName.equals( "Summary" ) || 
                         propertySetName.equals( "Brief" ) ) {
                    for ( int j = 0; j < nl.getLength(); j++ ) {
                        sb.append( DOMPrinter.nodeToString( nl.item( j ), "" ) );
                    }
                }

                sb.append( "</searchResult>" );
                allSB.append( sb );
            } else if ( results.get( i ) instanceof Exception ) {
                // write exception message to the client/destination and quit
                dest.write( ( (Exception)results.get( i ) ).getMessage() );
                results.clear();
                return;
            }
        }

        StringBuffer sb = new StringBuffer( allSB.length() + 200 );
        String time = TimeTools.getISOFormattedTime();
        sb.append( "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>" );
        sb.append( "<searchResponse success=\"true\" timestamp=\"" + time + "\" " );
        sb.append( "numberOfRecords=\"" + cntAll + "\" " );
        sb.append( "xmlns:gml=\"http://www.opengis.net/gml\">" );
        sb.append( allSB );
        sb.append( "</searchResponse>" );

        OGCWebServiceResponse resp = CASProtocolFactory.createCASGetRecordResponse( inRequest, null, 
                                                                                    sb.toString() );

        OGCWebServiceEvent event = new OGCWebServiceEvent_Impl( this, resp, "" );

        dest.write( event );

        Debug.debugMethodEnd();
    }

    /**
     * handles the creation and send of a describeRecordType response back
     * to the client
     */
    private void handleDescribeRecordTypeResponse() {
        Debug.debugMethodBegin( this, "handleDescribeRecordTypeResponse" );

        WFSDescribeFeatureTypeResponse res = (WFSDescribeFeatureTypeResponse)results.get( 0 );
        Document doc = res.getFeatureTypeSchema();

        String s = DOMPrinter.nodeToString( doc, "iso-8859-1" );
        OGCWebServiceResponse resp = CASProtocolFactory.createCASDescribeRecordTypeResponse( 
                                             inRequest, null, s );
        OGCWebServiceEvent event = new OGCWebServiceEvent_Impl( this, resp, "" );

        dest.write( event );

        Debug.debugMethodEnd();
    }

    /**
     * handles the creation and send of a transaction response back
     * to the client
     */
    private void handleTransactionResponse() {
        Debug.debugMethodBegin();

        for ( int i = 0; i < results.size(); i++ ) {
            Object o = results.get( i );

            if ( o instanceof Exception ) {
                // write exception message to the client/destination and quit
                dest.write( ( (Exception)results.get( i ) ).getMessage() );
                results.clear();
                return;
            } else {
                WFSTransactionResponse res = (WFSTransactionResponse)o;

                WFSInsertResult[] ir = res.getInsertResult();
                CASInsertResult[] cir = null;

                if ( ir != null ) {
                    cir = new CASInsertResult[ir.length];

                    for ( int k = 0; k < cir.length; k++ ) {
                        cir[k] = CASProtocolFactory.createCASInsertResult( ir[k].getHandle(), 
                                                                           ir[k].getFeatureIds() );
                    }
                }

				OGCWebServiceException ex = null;
				if (res.getException ()!= null) {
					 ex = new OGCWebServiceException_Impl( res.getException() );
				}
				OGCWebServiceResponse resp = CASProtocolFactory.createCASTransactionResponse( 
													 inRequest, ex, cir, res.getStatus(), 
													 res.getHandle() );
				OGCWebServiceEvent event = new OGCWebServiceEvent_Impl( this, resp, "" );
				dest.write( event );				
            }
        }

        Debug.debugMethodEnd();
    }

    /**
     * handles the creation and send of a registerService response back
     * to the client
     */
    private void handleRegisterServiceResponse() {
        Debug.debugMethodBegin();
        Debug.debugMethodEnd();
    }

    /**
     * handles the response to a GetCapabilities request
     */
    private void handleGetCapabilitiesResponse() {
        Debug.debugMethodBegin();

        dest.write( capa.exportAsXML() );

        Debug.debugMethodEnd();
    }

    /**
     * transforms a CASRequest to one or more WFS requests
     */
    private OGCWebServiceEvent[] transformToWFSRequest( OGCWebServiceRequest request )
                                                throws Exception {
        Debug.debugMethodBegin();

        OGCWebServiceEvent[] events = new OGCWebServiceEvent[0];

        if ( request instanceof CASGetRecordRequest ) {
            reqType = 1;
            events = transformGetRecord( (CASGetRecordRequest)request );
        } else if ( request instanceof CASDescribeRecordTypeRequest ) {
            reqType = 2;
            events = transformDescribeRecordType( (CASDescribeRecordTypeRequest)request );
        } else if ( request instanceof CASTransactionRequest ) {
            reqType = 3;
            events = transformTransaction( (CASTransactionRequest)request );
        } else if ( request instanceof CASRegisterServiceRequest ) {
            reqType = 4;
            events = transformRegisterService( (CASRegisterServiceRequest)request );
        } else if ( request instanceof CASGetCapabilitiesRequest ) {
            reqType = 5;
            handleGetCapabilities( (CASGetCapabilitiesRequest)request );
        }

        Debug.debugMethodEnd();
        return events;
    }

    /**
     * transforms a getRecord request to one or more getFeature requests
     * and encapsulates them into one or more <tt>OGCWebServiceEvent</tt>s
     */
    private OGCWebServiceEvent[] transformGetRecord( CASGetRecordRequest request )
                                             throws Exception {
        Debug.debugMethodBegin();

        // if the request shall be forwarded to federated catalogs
        // increase the number of expceted reponses
        if ( request.getQueryScope() > 0 ) {
            // performs requests against feaderated/cascaded catalogs
            try {
                performCascadeGetRecord( request );
            } catch ( Exception e ) {
                System.out.println( e );
            }
        }

        CASQuery[] queries = request.getQuery();      
        OGCWebServiceEvent[] events = new OGCWebServiceEvent[queries.length];
        OGCWebServiceRequest req = null;

        for ( int i = 0; i < queries.length; i++ ) {
            String propertySetName = queries[i].getPropertySetName();

            String[] props = null;

            if ( propertySetName.equals( "Hits" ) ) {
                props = new String[] { "COUNT(*)" };
            } else if ( propertySetName.equals( "Brief" ) ) {
                props = new String[] {
                    "MD_Metadata/fileIdentifier", 
                    "MD_Metadata/identificationInfo/MD_DataIdentification/citation/title", 
                };
            } else if ( propertySetName.equals( "Summary" ) ) {
            } else if ( propertySetName.equals( "Full" ) ) {
            }

            WFSQuery q = WFSProtocolFactory.createQuery( props, queries[i].getHandle(), 
                                                         queries[i].getVersion(), 
                                                         request.getOutputRecType(), 
                                                         queries[i].getFilter() );

            req = WFSProtocolFactory.createWFSGetFeatureRequest( queries[i].getVersion(), 
                                                                 request.getId() + "-" + i, null, 
                                                                 null, //request.getOutputFormat(),
            propertySetName, propertySetName, request.getFilter(), request.getMaxRecords(), 
                                                                 request.getStartPosition(), 
                                                                 new WFSQuery[] { q } );

            events[i] = new OGCWebServiceEvent_Impl( this, req, "", null );
        }

        Debug.debugMethodEnd();
        return events;
    }

    /**
     * transforms a describeRecordType request to one describeFeatureType request
     * and encapsulates it into one <tt>OGCWebServiceEvent</tt>
     */
    private OGCWebServiceEvent[] transformDescribeRecordType( CASDescribeRecordTypeRequest request ) {
        Debug.debugMethodBegin( this, "transformDescribeRecordType" );

        String[] types = request.getTypeNames();
        String[] sets = request.getSetNames();

        for ( int i = 0; i < types.length; i++ ) {
            if ( types[i].equalsIgnoreCase( "product" ) || 
                     types[i].equalsIgnoreCase( "collection" ) ) {
                if ( sets[i].equalsIgnoreCase( "full" ) ) {
                    types[i] = "ISO19115";
                } else {
                    types[i] = "ISO19115" + sets[i];
                }
            } else {
                if ( sets[i].equalsIgnoreCase( "full" ) ) {
                    types[i] = "ISO19119";
                } else {
                    types[i] = "ISO19119" + sets[i];
                }
            }
        }

        OGCWebServiceEvent[] event = new OGCWebServiceEvent[1];
        OGCWebServiceRequest req = WFSProtocolFactory.createWFSDescribeFeatureTypeRequest( 
                                           request.getVersion(), request.getId(), null, null, 
                                           request.getOutputFormat(), types );

        event[0] = new OGCWebServiceEvent_Impl( this, req, "", null );

        Debug.debugMethodEnd();
        return event;
    }

    /**
     * transforms a (WCAS) transaction request to one (WFS) transaction request
     * and encapsulates it into one <tt>OGCWebServiceEvent</tt>
     */
    private OGCWebServiceEvent[] transformTransaction( CASTransactionRequest request ) {
        Debug.debugMethodBegin( );

        OGCWebServiceEvent[] event = new OGCWebServiceEvent[1];

        CASOperation[] op = request.getOperations();
        WFSOperation[] wfsop = new WFSOperation[op.length];

        for ( int i = 0; i < op.length; i++ ) {
            String handle = null;

            if ( op[i] instanceof CASInsert ) {
                handle = ( (CASInsert)op[i] ).getHandle();
                Object[] o = ( (CASInsert)op[i] ).getMetadata();
                GMLFeature[] feat = new GMLFeature[o.length];

                for ( int j = 0; j < o.length; j++ ) {
                    feat[j] = (GMLFeature)o[i];
                }

                wfsop[i] = WFSProtocolFactory.createInsert( feat, handle );
            } else if ( op[i] instanceof CASDelete ) {
                Filter filter = ( (CASDelete)op[i] ).getFilter();
                String typeName = ( (CASDelete)op[i] ).getType();
                wfsop[i] = WFSProtocolFactory.createDelete( filter, typeName );
            } else if ( op[i] instanceof CASUpdate ) {
            }
        }

        OGCWebServiceRequest req = WFSProtocolFactory.createWFSTransactionRequest( 
                                           request.getVersion(), request.getId(), 
                                           request.getLockId(), wfsop, request.getHandle(), null );

        event[0] = new OGCWebServiceEvent_Impl( this, req, "", null );

        Debug.debugMethodEnd();
        return event;
    }

    /**
     * performs a registerService request. this will be done by requesting
     * the service describing XML document from the specified address and
     * transforming it into one or more (WFS) transaction requests
     * encapsulated into one or more <tt>OGCWebServiceEvent</tt>s.
     */
    private OGCWebServiceEvent[] transformRegisterService( CASRegisterServiceRequest request )
                                                   throws Exception {
        Debug.debugMethodBegin( this, "transformRegisterService" );

        URL url = request.getServiceAddress();
        InputStreamReader isr = new InputStreamReader( url.openStream() );
        Document doc = XMLTools.parse( isr );

        Debug.debugMethodEnd();
        return null;
    }

    /**
     * handles a get capabilities request by reading the capabilities
     * document from its source.
     */
    private void handleGetCapabilities( CASGetCapabilitiesRequest request ) {
        Debug.debugMethodBegin( this, "transformGetCapabilities" );
        Debug.debugMethodEnd();
    }

    /**
     * performs getRecord requests against all known federated
     * catalogs.
     */
    private void performCascadeGetRecord( CASGetRecordRequest request ) throws Exception {
        Debug.debugMethodBegin( this, "performCascadeGetRecord" );

        CASQuery[] queries = request.getQuery();
        StringBuffer sb = new StringBuffer( ( queries.length * 500 ) + 500 );
        sb.append( "<GetRecord xmlns:wfs=\"http://www.opengis.net/namespaces/wfs\" " );
        sb.append( "maxRecords=\"" + request.getMaxRecords() + "\" " );
        sb.append( "outputFormat=\"" + request.getOutputFormat() + "\" " );
        sb.append( "outputRecType=\"" + request.getOutputRecType() + "\" " );
        sb.append( "queryScope=\"" + request.getQueryScope() + "\" " );
        sb.append( "startPosition=\"" + request.getStartPosition() + "\" >" );

        for ( int i = 0; i < queries.length; i++ ) {
            sb.append( "<Query handle=\"" + queries[i].getHandle() + "\" " );
            sb.append( "typeName=\"" + queries[i].getTypeName() + "\" " );
            sb.append( "version=\"" + queries[i].getVersion() + "\">" );

            if ( queries[i].getPropertySetName() != null ) {
                sb.append( "<PropertySetName>" );
                sb.append( queries[i].getPropertySetName() );
                sb.append( "</PropertySetName>" );
            }

            sb.append( "</Query>" );
        }

        Filter filter = request.getFilter();

        if ( filter != null ) {
            sb.append( filter.toXML() );
        }

        FederatedCatalog[] fc = capa.getCapability().getFederatedCatalogs();

        // this is a quite simple not very fast and save way to handle cascading
        // catalog requests. it works for GDI NRW Testbed II but it have to be
        // enhanced for more generic versions of the catalog
        // TODO
        for ( int i = 0; i < fc.length; i++ ) {
            URL url = fc[i].getCatalogURL();
            NetWorker nw = new NetWorker( url, sb.toString() );
            InputStreamReader isr = new InputStreamReader( nw.getInputStream(), "UTF-8" );

            // increase number of expected responses to a request
            expectedResponses++;

            Document doc = XMLTools.parse( isr );
            NodeList nl = doc.getElementsByTagName( "iso19119Summary" );

            if ( nl != null ) {
                for ( int j = 0; j < nl.getLength(); j++ ) {
                    Element elem = (Element)nl.item( j );
                    elem.setAttribute( "federatedCatalog", url.toString() );
                }
            }

            nl = doc.getElementsByTagName( "iso19119Brief" );

            if ( nl != null ) {
                for ( int j = 0; j < nl.getLength(); j++ ) {
                    Element elem = (Element)nl.item( j );
                    elem.setAttribute( "federatedCatalog", url.toString() );
                }
            }

            WFSGetFeatureResponse res = WFSProtocolFactory.createWFSGetFeatureResponse( request, 
                                                                                        null, null, 
                                                                                        doc );
            OGCWebServiceEvent event = new OGCWebServiceEvent_Impl( this, res, "" );
            handleResponse( event );
        }

        Debug.debugMethodEnd();
    }

    /** handles a request against an OGC web service
     * @param event event object that contains the request to be performed
     */
    public void handleRequest( OGCWebServiceEvent event ) {
        Debug.debugMethodBegin();
        // call the dispatcher to handle the request
        OGCWebServiceRequest request = event.getRequest();

        if ( !( request instanceof CASGetCapabilitiesRequest ) ) {
            dispatcher.handleRequest( event );
        } else {
            try {
                handleGetCapabilitiesResponse();
            } catch ( Exception ex ) {
                Debug.debugException( ex, null );
                dest.write( ex );
            }
        }

        Debug.debugMethodEnd();
    }

    /** handles the response of an OGC web service
     * @param event event oject that contains the response to a request
     */
    public void handleResponse( OGCWebServiceEvent event ) {
        Debug.debugMethodBegin();
        
        if ( event.getResponse().getException() != null ) {
            String s = DOMPrinter.nodeToString( event.getResponse().getException(), "iso-8859-1" );
            results.add( new Exception( s ) );
        } else {
            results.add( event.getResponse() );
        }

        if ( results.size() == expectedResponses ) {
            try {
                // handles the response creation and sending in
                // depency to the request type that have been performed
                switch ( reqType ) {
                    case 1:
                        handleGetRecordResponse();
                        break;
                    case 2:
                        handleDescribeRecordTypeResponse();
                        break;
                    case 3:
                        handleTransactionResponse();
                        break;
                    case 4:
                        handleRegisterServiceResponse();
                        break;
                    default: {
                        OGCWebServiceException ex = 
                            new OGCWebServiceException_Impl( "WCASService_Impl:handleResponse",
                                                             "not known request type" );
                        OGCWebServiceResponse resp = CASProtocolFactory.createCASTransactionResponse( 
                                                             inRequest, ex, null, null, null );
                        event = new OGCWebServiceEvent_Impl( this, resp, "" );                        
                        dest.write( event );
                    }
                }
            } catch ( Exception e ) {
            	e.printStackTrace ();
                OGCWebServiceException ex = 
                    new OGCWebServiceException_Impl( "WCASService_Impl:handleResponse",
                                                     e.toString() );
                OGCWebServiceResponse resp = CASProtocolFactory.createCASTransactionResponse( 
                                                     inRequest, ex, null, null, null );
                event = new OGCWebServiceEvent_Impl( this, resp, "" );
                dest.write( event );
            }            
            // clear list of result after processing them
            results.clear();
        }

        Debug.debugMethodEnd();
    }

    /** returns true if the handler is interested in a event
     * @param event event object that contains a request or response to a request
     * @return true if the service is interested in handling the request/response contained
     * within the event object
     */
    public boolean isInterested( OGCWebServiceEvent event ) {
        if ( event.getType() == OGCWebServiceEvent.RESPONSE ) {
            String id1 = event.getId();

            for ( int i = 0; i < requests.length; i++ ) {
                String id2 = requests[i].getId();

                if ( id2.equals( id1 ) ) {
                    return true;
                }
            }

            return false;
        } else {
            return false;
        }
    }

    /** registers a Handler so this Handler is able to act as a proxy
     * to the registered handler
     * @param handler handler to be registered to the service
     */
    public void registerHandler( Handler handler ) {
        this.dispatcher = handler;
    }

    /** removes a registered handler from the service
     * @see WCASService_Impl#registerHandler(Handler)
     * @param handler handler to be removed from the service
     */
    public void removeHandler( Handler handler ) {
        if ( handler.equals( dispatcher ) ) {
            dispatcher = null;
        }
    }
}