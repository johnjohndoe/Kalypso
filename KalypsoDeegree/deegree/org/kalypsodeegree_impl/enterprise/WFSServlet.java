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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.io.StringReader;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.deegree.gml.GMLFeatureCollection;
import org.deegree.services.OGCWebService;
import org.deegree.services.OGCWebServiceClient;
import org.deegree.services.OGCWebServiceEvent;
import org.deegree.services.OGCWebServiceException;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.OGCWebServiceResponse;
import org.deegree.services.wfs.capabilities.WFSCapabilities;
import org.deegree.services.wfs.protocol.WFSDescribeFeatureTypeResponse;
import org.deegree.services.wfs.protocol.WFSGetCapabilitiesResponse;
import org.deegree.services.wfs.protocol.WFSGetFeatureResponse;
import org.deegree.services.wfs.protocol.WFSGetFeatureWithLockResponse;
import org.deegree.services.wfs.protocol.WFSLockFeatureResponse;
import org.deegree.services.wfs.protocol.WFSTransactionResponse;
import org.deegree.xml.DOMPrinter;
import org.deegree.xml.Marshallable;
import org.deegree_impl.gml.GMLFeatureCollection_Impl;
import org.deegree_impl.services.OGCWebServiceEvent_Impl;
import org.deegree_impl.services.OGCWebServiceException_Impl;
import org.deegree_impl.services.wfs.capabilities.WFSCapabilitiesFactory;
import org.deegree_impl.services.wfs.protocol.WFSProtocolFactory;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.StringExtend;
import org.w3c.dom.Document;


/**
 * Serves as an OCC-compliant HTTP-frontend to the WFS.
 * <p>
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider</a>
 * @version $Revision$ $Date$
 */
public class WFSServlet extends AbstractOGCServlet {

	protected WFSServicePool servicePool = null;
    protected WFSCapabilities wfsCapabilities = null;
    private Exception initException = null;


	/**
	 * Called by the server (via the service method) to allow a servlet to
	 * handle a GET request.
	 * @param req an HttpServletRequest object that contains the request the
	 *            client has made to the servlet 
	 * @param resp an HttpServletResponse object that contains the response
	 *             the servlet sends to the client
	 */
	public void doGet( HttpServletRequest req, HttpServletResponse resp ) {
		Debug.debugMethodBegin();

        if ( initException != null ) {
            handleError( initException, resp );
        } else {
            try {
                WFS wfs = new WFS (getParamMap (req), resp);
                wfs.perform ();
            } catch (Exception e) {
                handleError( e, resp );
            }
        }

		Debug.debugMethodEnd();
	}

	/**
	 * Called by the server (via the service method) to allow a servlet to
	 * handle a POST request.
	 * @param req an HttpServletRequest object that contains the request the
	 *            client has made to the servlet 
	 * @param resp an HttpServletResponse object that contains the response
	 *             the servlet sends to the client
	 */
	public void doPost( HttpServletRequest req, HttpServletResponse resp ) {
		Debug.debugMethodBegin( this, "doPost" );

        if ( initException != null ) {
            handleError( initException, resp );
        } else {
            try {
                WFS wfs = new WFS (req, resp);
                wfs.perform ();
            } catch (Exception e) {
                handleError( e, resp );
            }
        }

		Debug.debugMethodEnd();
	}

	/**
	 * Called by the servlet container to indicate that the servlet is being
	 * placed into service.
	 * @param servletConfig servlet capabilities
	 * @throws ServletException exception if something occurred that interferes
	 *         with the servlet's normal operation
	 */
	public void init( ServletConfig servletConfig ) throws ServletException {
		super.init( servletConfig );
		enableReloader();
	}

	/**
	 * Called when reinitialization of the service is necessary, e.g.
	 * the capabilities file has been altered.
	 */
	protected void initService() {
        initException = null;
		String capabilities = getInitParameter( "capabilities" );

		if ( capabilities == null ) {            
			getServletContext().log( "Parameter 'capabilities' unspecified. Exiting." );
			initException = new Exception( "Parameter 'capabilities' unspecified. Exiting." );
            return;
		}

		try {
			capabilitiesURL = new URL( capabilities );
		} catch ( MalformedURLException e ) {
			getServletContext().log( e.toString() );
			initException = e;
            return;
		}

		try {
			wfsCapabilities = WFSCapabilitiesFactory.createCapabilities( capabilitiesURL );

			// get the service-pool for WFS services
			if ( servicePool == null ) {
				servicePool = WFSServicePool.getInstance (wfsCapabilities );
			} else {
				// clear, destroy and re-initialize the service-pool if the
				// capabilities file of the WFS has been changed
				synchronized ( servicePool ) {
					servicePool.destroy();
					servicePool = WFSServicePool.getInstance(wfsCapabilities );
				}
			}

			// evaluate 'maxInstances' parameter
			try {
				String maxInstancesStr = getInitParameter( "maxInstances" );

				if ( maxInstancesStr == null ) {
					getServletContext ().log ("Parameter 'maxInstances' unspecified. Using default value." );
				} else {
					servicePool.setMaxInstances( Integer.parseInt( maxInstancesStr ) );
				}
			} catch ( NumberFormatException e ) {
				getServletContext ().log ("Parameter 'maxInstances' has invalid format. Using default value." );
			}

			// evaluate 'initInstances' parameter
			try {
				String initInstancesStr = getInitParameter( "initInstances" );

				if ( initInstancesStr == null ) {
					getServletContext ().log ("Parameter 'initInstances' unspecified. Using default value." );
				} else {
					servicePool.fill( Integer.parseInt( initInstancesStr ) );
				}
			} catch ( NumberFormatException e ) {
				getServletContext ().log ("Parameter 'initInstances' has invalid format. Using default value." );
			}
		} catch ( Exception e ) {
			getServletContext().log( e.toString() );
			initException = e;
		}        
	}

    //////////////////////////////////////////////////////////////////////////
    //                           inner classes                              //
    //////////////////////////////////////////////////////////////////////////

    /**
     * private inner class that represents the web feature service.
     */
    private class WFS implements OGCWebServiceClient {
        private HttpServletResponse servletResponse = null;
        private String request = null;
        private OGCWebService service = null;
        private boolean finished = false;
        
        /** constructor
         */
        WFS( HttpServletRequest servletRequest, HttpServletResponse servletResponse )
            throws Exception {
            finished = false;
            this.servletResponse = servletResponse;
            request = getPostContent( servletRequest );
        }

        /**
         * Creates a new WFS object.
         *
         * @param model 
         * @param servletResponse 
         *
         * @throws Exception 
         */
        WFS( HashMap model, HttpServletResponse servletResponse ) throws Exception {
            this.servletResponse = servletResponse;
            finished = false;
            String req = (String)model.get( "REQUEST" );

            if ( req.equals( "DescribeFeatureType" ) ) {
                // create an xml-encoding of the DescribeFeatureType request
                StringBuffer sb = new StringBuffer( 500 );
                sb.append( "<DescribeFeatureType outputFormat=" );
                sb.append( "\"" + model.get( "OUTPUTFORMAT" ) + "\">" );

                String tmp = (String)model.get( "TYPENAME" );

                if ( tmp != null ) {
                    String[] typeNames = StringExtend.toArray( tmp, ",;", true );
                    for ( int i = 0; i < typeNames.length; i++ ) {
                        sb.append( "<TypeName>" ).append( typeNames[i] );
                        sb.append( "</TypeName>" );
                    }
                } else {
                    org.deegree.services.wfs.capabilities.FeatureType[] ft = 
                        wfsCapabilities.getFeatureTypeList().getFeatureTypes();
                    for ( int i = 0; i < ft.length; i++ ) {
                        sb.append( "<TypeName>" ).append( ft[i].getName() );
                        sb.append( "</TypeName>" );
                    }
                }

                sb.append( "</DescribeFeatureType>" );
                request = sb.toString();
            } else if ( req.equals( "GetCapabilities" ) ) {
                // create an xml-encoding of the GetCapabilities request
                StringBuffer sb = new StringBuffer( 100 );
                sb.append( "<GetCapabilities version=" );
                sb.append( "\"" + model.get( "VERSION" ) + "\"/>" );
                request = sb.toString();
            } else {
                OGCWebServiceException exception = new OGCWebServiceException_Impl( "WFSServlet", 
                                                                                    req + " is " + 
                                                                                    "no valid WFS request" );
                write( exception.toString() );

            }
        }

        /**
         * returns the content of the http post request without its header
         */
        private String getPostContent( HttpServletRequest request ) throws IllegalArgumentException, 
                                                                           IOException {
            Debug.debugMethodBegin( this, "getPostContent" );

            BufferedReader br = request.getReader();

            StringBuffer sb = new StringBuffer( 2000 );
            String line = null;

            while ( ( line = br.readLine() ) != null ) {
                sb.append( line );
            }

            br.close();
            Debug.debugMethodEnd();
            return sb.toString();
        }

        /**
         * performs the request by calling the <tt>doService</tt> method
         * of a <tt>WFSService</tt> instance.
         */
        protected void perform() {
            String id = "id-" + Math.random();

            try {
                StringReader sr = new StringReader( request );
                OGCWebServiceRequest request = WFSProtocolFactory.createRequest( id, sr );
                sr.close();

                OGCWebServiceEvent event = new OGCWebServiceEvent_Impl( this, request, null, this );

                // acuire a WFSService from the pool
                service = (OGCWebService)servicePool.acuireObject();
                service.doService( event );
                int timeLimit = 1000 * 60 * 5;
                long timeStamp = System.currentTimeMillis();
//                synchronized (this) {
//                    try {
//                        wait( timeLimit );
//                    } catch (Exception e) {}
//                }
                // wait until the response to the request has received
                // max waiting time is two minutes
                waitForFinish( timeLimit );
                timeStamp = System.currentTimeMillis() - timeStamp;
                if ( timeStamp > timeLimit ) {
                    StringBuffer sb = new StringBuffer(200);
                    sb.append( "<OGCWebServiceException>" );        
                    sb.append( "<Exception><Message>" );
                    sb.append( "Request processing exceeds time limit" );        
                    sb.append( "</Message>" );        
                    sb.append( "<Locator>WFSServlet:WFS:perform</Locator>" );
                    sb.append( "</Exception></OGCWebServiceException>" ); 
                    write( sb.toString() );
                }
            } catch ( Exception e ) {
                Debug.debugException( e, null );
            }
        }
        
        /**
         * the method stops the processing for the submitted
         * amount of minutes. The processing will also be continued
         * if a other method sets the finished parameter to true.
         * if an error raises while waiting or the loop is finished
         * by exceeding the time the method returns <tt>false</tt>
         * otherwise <tt>true</tt> will be returned.
         */
        private boolean waitForFinish(int minute) {
            Debug.debugMethodBegin( this, "waitForFinish");
            
            boolean ok = true;
            long start = System.currentTimeMillis();
            long runtime = start;
            while (!finished) {
                try {
                    Thread.sleep(100);
                } catch(Exception e) {
                    ok = false;
                    finished = true;
                    Debug.debugException( e, " - ");
                }
                runtime += 100;
                // finish loop after "minute" minutes if request hasn't be
                // answered
                if ( runtime - start > 1000*60*minute) {
                    finished = true;
                    ok = false;
                }
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

            try {
                // release WFSService back to the pool
                servicePool.releaseObject( service );

                OutputStream os = servletResponse.getOutputStream();
                servletResponse.setContentType( "text/xml" );
                if ( result instanceof String ) {
                    PrintStream bos = new PrintStream( os );
                    bos.print( (String)result );
                    os.close();
                    bos.close();
                } else if ( result instanceof OGCWebServiceEvent ) {
                    OGCWebServiceResponse res = ((OGCWebServiceEvent)result).getResponse();
                    Document doc = res.getException();
                    if ( doc != null ) {      
                        String s = DOMPrinter.nodeToString( doc, "UTF-8" );
                        Writer osw = new BufferedWriter( new OutputStreamWriter( os, "UTF-8" ) );
                        osw.write( s );
                        osw.close();
                    } else {
                        if ( res instanceof WFSGetFeatureResponse ||
                             res instanceof WFSGetFeatureWithLockResponse ) {
                            if ( res.getException() != null ) {
                                returnException( os, res.getException() );
                            } else {
                                Object o = ((WFSGetFeatureResponse)res).getResponse();
                                if ( o instanceof GMLFeatureCollection ) {                                    
                                    GMLFeatureCollection_Impl gfc = (GMLFeatureCollection_Impl)o;
                                    String s = DOMPrinter.nodeToString( gfc.getAsElement().getOwnerDocument(), "UTF-8" );
                                    //servletResponse.setContentLength( s.length() * 2);
                                    Writer osw = new BufferedWriter( new OutputStreamWriter( os, "UTF-8" ) );
                                    osw.write( s );
                                    osw.close();
                                } else { 
                                    ObjectOutputStream oos = new ObjectOutputStream( os );
                                    oos.writeObject( o );
                                    os.close();
                                    oos.close();
                                }
                            }
                        } else if ( res instanceof WFSDescribeFeatureTypeResponse ) {
                            if ( res.getException() != null ) {
                                returnException( os, res.getException() );
                            } else {
                                doc = ((WFSDescribeFeatureTypeResponse)res).getFeatureTypeSchema();
                                String s = DOMPrinter.nodeToString( doc, "UTF-8" );
                                //servletResponse.setContentLength( s.length() * 2 );
                                Writer osw = new BufferedWriter(  new OutputStreamWriter( os, "UTF-8" ) );
                                osw.write( s );
                                osw.close();
                            }                        
                        } else if ( res instanceof WFSGetCapabilitiesResponse ) {
                            if ( res.getException() != null ) {
                                returnException( os, res.getException() );
                            } else {
                                Object o = ((WFSGetCapabilitiesResponse)res).getResponse();
                                if ( o instanceof WFSCapabilities ) {
                                    String s = ((WFSCapabilities)o).exportAsXML();
                                    //servletResponse.setContentLength( s.length() * 2 );
                                    Writer osw = new BufferedWriter( new OutputStreamWriter( os, "UTF-8" ) );
                                    osw.write( s );
                                    osw.close();
                                }  else {
                                    os.close();
                                }
                            }
                        } else if ( res instanceof WFSTransactionResponse ) {
                            if ( res.getException() != null ) {
                                returnException( os, res.getException() );
                            } else {
                                String s = ((Marshallable)res).exportAsXML();
                                //servletResponse.setContentLength( s.length() * 2 );
                                Writer osw = new BufferedWriter( new OutputStreamWriter( os, "UTF-8" ) );
                                osw.write( s );
                                osw.close();
                            }
                        } else if ( res instanceof WFSLockFeatureResponse ) {
                        }
                    }
                } else {
                    System.out.println("wrong result type: " + result.getClass().getName() );
                }

                System.out.println( "response sent ..." );
            } catch ( Exception ex ) {
                System.out.println( ex );
            }
            
            try {
                this.notifyAll();
            } catch (Exception e) {}
            Thread.currentThread().interrupt();
            finished = true;

        }
    }
    
    protected void returnException(OutputStream os, Document exception) throws IOException {
        String s = DOMPrinter.nodeToString( exception, "iso-8859-1" );
        os.write( s.getBytes() );
        os.close();
    }
}