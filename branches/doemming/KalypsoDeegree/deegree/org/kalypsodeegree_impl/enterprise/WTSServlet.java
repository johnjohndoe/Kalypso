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

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.net.URL;
import java.util.Enumeration;
import java.util.HashMap;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.deegree.graphics.Encoders;
import org.deegree.services.InconsistentRequestException;
import org.deegree.services.OGCWebService;
import org.deegree.services.OGCWebServiceClient;
import org.deegree.services.OGCWebServiceEvent;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.WebServiceException;
import org.deegree.services.wts.protocol.WTSGetViewResponse;
import org.deegree_impl.services.OGCWebServiceEvent_Impl;
import org.deegree_impl.services.wts.protocol.WTSProtocolFactory;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.IDGenerator;
import org.deegree_impl.tools.StringExtend;


/**
 * Serves as an OCC-compliant HTTP-frontend to the WTS.
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider</a>
 * @version $Revision$ $Date$
 */
public class WTSServlet extends AbstractOGCServlet {
    // Pool of instances of the underlying WTS-Service.
    protected WTServicePool servicePool = null;
    private Exception initException = null;
    //private WTS wts = null;

    /**  
     * Called by the server (via the service method) to allow a servlet to
     * handle a GET request.
     * @param req an HttpServletRequest object that contains the request the
     *            client has made to the servlet
     * @param resp an HttpServletResponse object that contains the response
     *             the servlet sends to the client
     */
    public void doGet( HttpServletRequest req, HttpServletResponse resp ) {
        Debug.debugMethodBegin( this, "doGet" );

        if ( initException != null ) {
            handleError( initException, resp );
        } else {
            try {
                new WTS( mapParams( req ), resp );
            } catch ( Exception e ) {            
                handleError( e, resp );
            }
        }

        Debug.debugMethodEnd();
    }
    
    private HashMap mapParams(HttpServletRequest request) {
        
        Enumeration enum = request.getParameterNames();
        HashMap map = new HashMap();
        while ( enum.hasMoreElements() ) {
            String key = ((String)enum.nextElement());
            Object value = request.getParameter(key );
            key = key.toUpperCase();
            if ( key.equals( "TEXTURE") || key.equals( "STYLES") || 
                 key.equals( "ELEVATION_MODELS") || key.equals( "FEATURE_COLLECTIONS") || 
                 key.equals( "POI") ) {
                value = StringExtend.toArray( value.toString(), ",",  false );
            }
            map.put( key, value );    
        }

        return map;
    }

    /**
     * Called by the servlet container to indicate that the servlet is being
     * placed into service.
     * @param servletConfig servlet configuration
     * @throws ServletException exception if something occurred that interferes
     *         with the servlet's normal operation
     */
    public void init( ServletConfig servletConfig ) throws ServletException {
        Debug.debugMethodBegin( );
        super.init( servletConfig );
        enableReloader();
        Debug.debugMethodEnd();
    }

    /** 
     * Called when reinitialization of the service is necessary, e.g.
     * the configuration file was altered.
     */
    protected void initService() {
        Debug.debugMethodBegin( );
        
        initException = null;

        String capabilities = getInitParameter( "capabilities" );

        if ( capabilities == null ) {
            Debug.debugSimpleMessage( "Parameter 'capabilities' unspecified. Exiting." );
            initException = new Exception( "Parameter 'capabilities' unspecified. Exiting." );
            Debug.debugMethodEnd();
            return;
        }

        try {
            capabilitiesURL = new URL( capabilities );
            //WTSConfigurationFactory.createConfiguration( capabilities );
        } catch ( Exception e ) {
            e.printStackTrace();
            getServletContext().log( e.toString() );
            initException = e;
            Debug.debugMethodEnd();
            return;
        }

        try {
            // get the service-pool for WTS services
            if ( servicePool == null ) {
                servicePool = WTServicePool.getInstance( capabilitiesURL );
            } else {
                // clear, destroy and re-initialize the service-pool if the 
                // configuration of the WTS has been changed                                
                synchronized ( servicePool ) {
                    servicePool.destroy();
                    servicePool = WTServicePool.getInstance( capabilitiesURL );
                }
            }

            // evaluate 'maxInstances' parameter
            try {
                String maxInstancesStr = getInitParameter( "maxInstances" );

                if ( maxInstancesStr == null ) {
                    Debug.debugSimpleMessage( 
                            "Parameter 'maxInstances' unspecified. Using default value." );
                } else {
                    servicePool.setMaxInstances( Integer.parseInt( maxInstancesStr ) );
                }
            } catch ( NumberFormatException e ) {
                Debug.debugSimpleMessage( 
                        "Parameter 'maxInstances' has invalid format. Using default value." );
            }

            // evaluate 'initInstances' parameter
            try {
                String initInstancesStr = getInitParameter( "initInstances" );

                if ( initInstancesStr == null ) {
                    Debug.debugSimpleMessage( 
                            "Parameter 'initInstances' unspecified. Using default value." );
                } else {
                    servicePool.fill( Integer.parseInt( initInstancesStr ) );
                }
            } catch ( NumberFormatException e ) {
                Debug.debugSimpleMessage( 
                        "Parameter 'initInstances' has invalid format. Using default value." );
            }
        } catch ( Exception e ) {
            e.printStackTrace();
            getServletContext().log( e.toString() );
            initException = e;
        }

        Debug.debugMethodEnd();
    }

    /**
     * 
     *
     * @version $Revision$
     * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
     */
    private class WTS implements OGCWebServiceClient {
        private HttpServletResponse resp = null;
        private OGCWebServiceRequest request = null;
        private OGCWebService service = null;
        private boolean finished = false;

        /**
         * FIXME: Exception type when pool is empty!
         */
        private WTS( HashMap paramMap, HttpServletResponse resp )
             throws InconsistentRequestException, WebServiceException {
            this.resp = resp;

            try {
                String id = "id-" + IDGenerator.getInstance().generateUniqueID();
                request = WTSProtocolFactory.createRequest( id, paramMap );
            } catch ( Exception e ) {
                throw new InconsistentRequestException( e.toString() );
            }

            OGCWebServiceEvent event = new OGCWebServiceEvent_Impl( this, request, null, this );

            try {
                // acuire a WTService from the pool
                service = (OGCWebService)servicePool.acuireObject();                

                if ( service == null ) {
                    throw new WebServiceException( "No WTService available through the pool" );
                }

                service.doService( event );
                waitForFinish( 5 );
            } catch ( Exception e ) {
                Debug.debugException( e, "" );
            }
        }

        /**
         * Stops the processing for the given number of minutes. The processing
         * will be continued if another Thread sets the finished parameter first.
         * FIXME: Better use signals instead of polling.
         * @param mins the number of minutes to suspend execution
         * @return true, if the finished parameter has been set "in time", else
         *         false
         */
        private boolean waitForFinish( int mins ) {
            Debug.debugMethodBegin( this, "waitForFinish" );
            
            boolean ok = true;
            long runtime = 0;
            long timeLimit = 1000 * 60 * mins;

            while ( !finished ) {
                try {
                    Thread.sleep( 100 );
                } catch ( Exception e ) {
                    ok = false;
                    finished = true;
                    Debug.debugException( e, " - " );
                }

                runtime += 100;

                // finish loop after "minute" minutes if request hasn't be
                // answered
                if ( runtime > timeLimit ) {
                    finished = true;
                    ok = false;
                }
            }
            
            try {
                servicePool.releaseObject( service );
            } catch(Exception e) {
                System.out.println(e);	
            }

            Debug.debugMethodEnd();
            return ok;
        }

        /**
         *
         *
         * @param result 
         */
        public void write( Object result ) {
            
            OGCWebServiceEvent event = (OGCWebServiceEvent)result;
            WTSGetViewResponse res = (WTSGetViewResponse)event.getResponse();
            BufferedImage bi = (BufferedImage)res.getView();
            try {
                OutputStream os = resp.getOutputStream();
                Encoders.encodeJpeg( os, bi, 0.90f );
                os.close();
            } catch(Exception e) {
                e.printStackTrace();	
                PrintWriter out = null;

                try {
                    out = resp.getWriter();
                } catch ( IOException ex ) {
                }

                resp.setContentType( "text/html" );
                out.println( "<HTML>" );
                out.println( "<HEAD><TITLE>WTS-Servlet</TITLE></HEAD>" );
                out.println( "<BODY BGCOLOR=\"#CFFFDF\">" );
                out.println( "<TT><H1>Inconsistent request:</H1>" );
                out.println( "<BLINK>" + e.getMessage() + "</BLINK></TT>" );
                out.println( "</BODY>" );
                out.println( "</HTML>" );
                out.close();
            }
            finished = true;
            bi = null;
        }
    }
}
