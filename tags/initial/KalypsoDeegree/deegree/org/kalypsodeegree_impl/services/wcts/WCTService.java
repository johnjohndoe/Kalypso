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
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.StringReader;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.deegree.services.OGCWebServiceClient;
import org.deegree.services.OGCWebServiceEvent;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.OGCWebServiceResponse;
import org.deegree.services.WebServiceException;
import org.deegree.services.wcts.protocol.DescribeTransformationRequest;
import org.deegree.services.wcts.protocol.DescribeTransformationResponse;
import org.deegree.services.wcts.protocol.GetCapabilitiesRequest;
import org.deegree.services.wcts.protocol.GetCapabilitiesResponse;
import org.deegree.services.wcts.protocol.IsTransformableRequest;
import org.deegree.services.wcts.protocol.IsTransformableResponse;
import org.deegree.services.wcts.protocol.TransformRequest;
import org.deegree.services.wcts.protocol.TransformResponse;
import org.deegree.xml.DOMPrinter;
import org.deegree_impl.services.OGCWebServiceEvent_Impl;
import org.deegree_impl.services.OGCWebService_Impl;
import org.deegree_impl.services.wcts.protocol.DescribeTransformationResponse_Impl;
import org.deegree_impl.services.wcts.protocol.TransformResponse_Impl;
import org.deegree_impl.services.wcts.protocol.WCTS_ProtocolFactory;
import org.deegree_impl.tools.Debug;


/**
 * This is the web coordinate transformation server class within the deegree
 * framework. A <tt>WCTService_Impl</tt> extends the <tt>WCTService</tt> interface
 * to act like a OGC web service. This means that a WCTS is callable through the
 * <tt>doService</tt>-method inherited from <tt>OGCWebService</tt>.
 *
 *
 * <p>--------------------------------------------------------------------</p>
* @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version 2002-08-07
 */
public class WCTService extends OGCWebService_Impl implements OGCWebServiceClient {
    private HttpServletResponse response = null;

    /**
     * default constructor
     */
    WCTService() {
    }

    /**
     * constructor
     * @params HttpServletRequest, HttpServletResponse
     */
    public WCTService( HttpServletRequest request, HttpServletResponse response ) {
        this.response = response;

        try {
            StringReader sr = new StringReader( getPostContent( request ) );

            OGCWebServiceRequest req = WCTS_ProtocolFactory.createRequest( "1", sr );

            OGCWebServiceEvent unser_request = new OGCWebServiceEvent_Impl( this, req, 
                                                                            "dies ist ein test", 
                                                                            this );

            doService( unser_request );
        } catch ( Exception ex ) {
            System.out.println( ex );
        }
    }

    /**
     * returns the content of the http post request without its header
     */
    private String getPostContent( HttpServletRequest request ) throws Exception {
        Debug.debugMethodBegin( this, "getPostContent" );

        InputStream is = request.getInputStream();

        int cnt = 100000;

        if ( is.available() > 0 ) {
            cnt = is.available();
        }

        StringBuffer sb = new StringBuffer( cnt );

        BufferedReader bf = new BufferedReader( new InputStreamReader( is ) );
        String line = null;

        while ( ( line = bf.readLine() ) != null ) {
            sb.append( line.trim() );
        }

        bf.close();

        String s = sb.toString();

        Debug.debugMethodEnd();
        return s;
    }

    /**
     * implements the <tt>doService</tt> method inherited from the
     * <tt>OGCWebService</tt> interface via <tt>WCTService<tt>. The method
     * analyses the request and calls the 4 ProtocolHandlers for performing it.
     */
    public void doService( OGCWebServiceEvent request ) throws WebServiceException {
        // "XML-Dokument" wird aus dem Request "herausgezogen"
        OGCWebServiceRequest wsr_request = request.getRequest();

        // sucht passenden handler
        if ( wsr_request instanceof GetCapabilitiesRequest ) {
            CapabilitiesHandler gcrh = new CapabilitiesHandler();
            gcrh.handleRequest( request );
        } else if ( wsr_request instanceof IsTransformableRequest ) {
            IsTransformableHandler itrh = new IsTransformableHandler();
            itrh.handleRequest( request );
        } else if ( wsr_request instanceof DescribeTransformationRequest ) {
            DescribeTransformationHandler dtrh = new DescribeTransformationHandler();
        } else if ( wsr_request instanceof TransformRequest ) {
            TransformHandler trh = new TransformHandler();
            trh.handleRequest( request );
        }
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
     * gets the response (result) and informs the servlet
     */
    public void write( Object result ) {
        OGCWebServiceEvent event = (OGCWebServiceEvent)result;

        try {
            String s = "";
            OutputStream os = response.getOutputStream();

            // GetCapabilities
            if ( event.getResponse() instanceof GetCapabilitiesResponse ) {
                GetCapabilitiesResponse res = (GetCapabilitiesResponse)event.getResponse();

                if ( res.getException() != null ) {
                    // String, containing the response-exception
                    s = res.getException().getDocumentElement().toString();
                } else {
                    // String, containing the response.
                    s = res.getCapabilities();
                }
                // IsTransformable
            } else if ( event.getResponse() instanceof IsTransformableResponse ) {
                IsTransformableResponse res = (IsTransformableResponse)event.getResponse();

                if ( res.getException() != null ) {
                    s = res.getException().getDocumentElement().toString();
                } else {
                    // transforming the boolean "Transformable" to a string
                    s = "" + res.getTransformable();
                }
                // DescribeTransformation
            } else if ( event.getResponse() instanceof DescribeTransformationResponse ) {
                DescribeTransformationResponse res = (DescribeTransformationResponse)event.getResponse();

                if ( res.getException() != null ) {
                    s = res.getException().getDocumentElement().toString();
                } else {
                    s = ( (DescribeTransformationResponse_Impl)res ).exportAsXML();
                }
                // Transform
            } else if ( event.getResponse() instanceof TransformResponse ) {
                TransformResponse res = (TransformResponse)event.getResponse();

                if ( res.getException() != null ) {
                    s = res.getException().getDocumentElement().toString();
                } else {
                    s = DOMPrinter.nodeToString( ( (TransformResponse_Impl)res ).exportAsXML(), 
                                                 "iso-8859-1" );
                }
            }

            os.write( s.getBytes() );
            os.close();
        } catch ( Exception ex ) {
            System.out.println( ex );
        }
    }
    
}