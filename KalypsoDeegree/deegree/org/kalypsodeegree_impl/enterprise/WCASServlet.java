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
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.Writer;
import java.net.URL;
import java.util.Enumeration;
import java.util.HashMap;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.deegree.services.OGCWebService;
import org.deegree.services.OGCWebServiceClient;
import org.deegree.services.OGCWebServiceEvent;
import org.deegree.services.OGCWebServiceException;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.OGCWebServiceResponse;
import org.deegree.services.wcas.capabilities.WCASCapabilities;
import org.deegree.services.wcas.protocol.CASGetRecordResponse;
import org.deegree.services.wcas.protocol.CASTransactionResponse;
import org.deegree.services.wfs.capabilities.WFSCapabilities;
import org.deegree.xml.DOMPrinter;
import org.deegree.xml.Marshallable;
import org.deegree_impl.services.OGCWebServiceEvent_Impl;
import org.deegree_impl.services.OGCWebServiceException_Impl;
import org.deegree_impl.services.wcas.capabilities.WCASCapabilitiesFactory;
import org.deegree_impl.services.wcas.protocol.CASProtocolFactory;
import org.deegree_impl.services.wfs.capabilities.WFSCapabilitiesFactory;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.StringExtend;
import org.w3c.dom.Document;

/**
 * 
 * <p>
 * --------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class WCASServlet extends HttpServlet
{

  private Reloader reloader = null;

  protected String capabilitiesFile = null;

  protected URL url = null;

  private WCASCapabilities capa = null;

  protected WCASServicePool pool = null;

  private WFSCapabilities wfsCapa = null;

  protected int freq = 60;

  private int initInstances = 1;

  private int maxInstances = 30;

  protected long capaTimestamp = 0;

  private Exception initException = null;

  /**
   * init-method of the servlet. only once called
   * 
   * @param servletConfig
   *          servlet configuration
   * @throws ServletException
   *           exception
   */
  public void init( ServletConfig servletConfig ) throws ServletException
  {
    super.init( servletConfig );

    Debug.setLevel( getInitParameter( "debug" ) );

    // get update frequency. if no valid value can be read 60 sec will be
    // used as default
    try
    {
      freq = Integer.parseInt( getInitParameter( "updateFrequency" ) );
    }
    catch( Exception e )
    {}

    // get number of initial WCAS instances to create. if no valid value can
    // be read 1 will be used as default
    try
    {
      initInstances = Integer.parseInt( getInitParameter( "initInstances" ) );
    }
    catch( Exception e )
    {}

    // get maximal size of the WCAS pool
    // if no max size of the pool is defined 30 will be used as default
    try
    {
      maxInstances = Integer.parseInt( getInitParameter( "maxInstances" ) );
    }
    catch( Exception ex )
    {}

    capabilitiesFile = getInitParameter( "wfsCapabilities" );

    try
    {
      URL url_ = new URL( capabilitiesFile );
      wfsCapa = WFSCapabilitiesFactory.createCapabilities( url_ );
    }
    catch( Exception ex )
    {
      getServletContext().log( ex.toString() );
      initException = ex;
      return;
    }

    capabilitiesFile = getInitParameter( "capabilities" );

    try
    {
      url = new URL( capabilitiesFile );

      File file = new File( url.getFile() );
      capaTimestamp = file.lastModified();
      capa = WCASCapabilitiesFactory.createCapabilities( url );
    }
    catch( Exception ex )
    {
      getServletContext().log( ex.toString() );
      initException = ex;
      return;
    }

    initWCAS();

    // initialize a class that checks if the capabilities of the WFS have
    // been changed and - if so - clears the WFSPool and reloads the
    // capabilities
    reloader = new Reloader();
    reloader.start();
  }

  /**
   * initializes the WCAS
   */
  protected void initWCAS()
  {
    Debug.debugMethodBegin( this, "initWCAS" );

    try
    {
      if( pool == null )
      {
        pool = WCASServicePool.getInstance( capa, wfsCapa );
      }
      else
      {
        capa = WCASCapabilitiesFactory.createCapabilities( url );
        synchronized( pool )
        {
          pool.destroy();
          pool = WCASServicePool.getInstance( capa, wfsCapa );
        }
      }

      pool.setMaxInstances( maxInstances );

      // initialize X WCASServices to the pool
      pool.fill( initInstances );
    }
    catch( Exception e )
    {
      getServletContext().log( e.toString() );
      initException = e;
    }

    Debug.debugMethodEnd();
  }

  /**
   * performs a http-post request
   */
  public void doPost( HttpServletRequest request, HttpServletResponse response )
  {
    Debug.debugMethodBegin( this, "doPost" );

    if( initException != null )
    {
      handleError( initException, response );
    }
    else
    {
      try
      {
        WCAS wcas = new WCAS( request, response );
        wcas.perform();
      }
      catch( Exception ex )
      {
        handleError( ex, response );
      }
    }

    Debug.debugMethodEnd();
  }

  /**
   *  
   */
  public void doGet( HttpServletRequest request, HttpServletResponse response )
  {
    Debug.debugMethodBegin( this, "doGet" );

    if( initException != null )
    {
      handleError( initException, response );
    }
    else
    {
      HashMap param = toModel( request );
      try
      {
        WCAS wcas = new WCAS( param, response );
        wcas.perform();
      }
      catch( Exception ex )
      {
        handleError( ex, response );
      }
    }

    Debug.debugMethodEnd();
  }

  /**
   * handles fatal errors by creating a OGC exception XML and sending it back to
   * the client
   */
  private void handleError( Exception ex, HttpServletResponse response )
  {
    String tmp = StringExtend.stackTraceToString( ex.getStackTrace() );
    getServletContext().log( tmp );
    OGCWebServiceException wex = new OGCWebServiceException_Impl( this.getClass().getName(), tmp );
    try
    {
      PrintWriter pw = response.getWriter();
      pw.write( ( (Marshallable)wex ).exportAsXML() );
      pw.close();
    }
    catch( Exception e )
    {
      getServletContext().log( e.toString() );
    }
  }

  /**
   * 
   * 
   * @param request
   * 
   * @return
   */
  private synchronized HashMap toModel( HttpServletRequest request )
  {
    HashMap param = new HashMap();

    Enumeration enum = request.getParameterNames();

    while( enum.hasMoreElements() )
    {
      String name = (String)enum.nextElement();
      String value = request.getParameter( name );
      param.put( name.toUpperCase(), value );
    }

    return param;
  }

  //////////////////////////////////////////////////////////////////////////
  //                           inner classes //
  //////////////////////////////////////////////////////////////////////////

  /**
   * private inner class that represents the web feature service.
   */
  private class WCAS implements OGCWebServiceClient
  {
    private HttpServletResponse servletResponse = null;

    private OGCWebService service = null;

    private String request = null;

    /**
     * constructor
     */
    WCAS( HttpServletRequest servletRequest, HttpServletResponse servletResponse ) throws Exception
    {
      this.servletResponse = servletResponse;
      request = getPostContent( servletRequest );
    }

    /**
     * Creates a new WCAS object.
     * 
     * @param model
     * @param servletResponse
     * 
     * @throws Exception
     */
    WCAS( HashMap model, HttpServletResponse servletResponse ) throws Exception
    {
      this.servletResponse = servletResponse;

      String req = (String)model.get( "REQUEST" );

      if( req.equals( "DescribeFeatureType" ) )
      {
        // create an xml-encoding of the DescribeFeatureType request
        StringBuffer sb = new StringBuffer( 500 );
        sb.append( "<DescribeRecordType outputFormat=" );
        sb.append( "\"" + model.get( "OUTPUTFORMAT" ) + "\">" );

        String tmp = (String)model.get( "TYPENAME" );

        if( tmp != null )
        {
          String[] typeNames = StringExtend.toArray( tmp, ",;", true );
          String[] setNames = StringExtend.toArray( (String)model.get( "SETNAME" ), ",;", false );

          for( int i = 0; i < typeNames.length; i++ )
          {
            sb.append( "<TypeName" ).append( "setName=\"" );
            sb.append( setNames[i] + "\">" );
            sb.append( typeNames[i] );
            sb.append( "</TypeName>" );
          }
        }

        sb.append( "</DescribeRecordType>" );
        request = sb.toString();
      }
      else if( req.equals( "GetCapabilities" ) )
      {
        // create an xml-encoding of the GetCapabilities request
        StringBuffer sb = new StringBuffer( 200 );
        sb.append( "<GetCapabilities version=" );
        sb.append( "\"" + model.get( "VERSION" ) + "\"/>" );
        request = sb.toString();
      }
    }

    /**
     * returns the content of the http post request without its header
     */
    private String getPostContent( HttpServletRequest request ) throws IllegalArgumentException,
        IOException
    {
      Debug.debugMethodBegin( this, "getPostContent" );

      BufferedReader br = request.getReader();
      StringBuffer sb = new StringBuffer( 2000 );
      String line = null;

      while( ( line = br.readLine() ) != null )
      {
        sb.append( line );
      }

      br.close();

      Debug.debugMethodEnd();
      return sb.toString();
    }

    /**
     * performs the request by calling the <tt>doService</tt> method of a
     * <tt> WCASService</tt> instance.
     */
    protected void perform()
    {
      String id = "id-" + Math.random();

      try
      {
        if( request.indexOf( "<GetCapabilities" ) > -1 )
        {
          StringBuffer sb = new StringBuffer( 100000 );

          try
          {
            URL url = new URL( capabilitiesFile );
            InputStream is = url.openStream();
            int c = -1;
            while( ( c = is.read() ) > -1 )
            {
              sb.append( (char)c );
            }

            is.close();
          }
          catch( Exception e )
          {
            OutputStream os = servletResponse.getOutputStream();
            os.write( e.toString().getBytes() );
            os.close();
          }

          try
          {
            OutputStream os = servletResponse.getOutputStream();
            os.write( sb.toString().getBytes() );
            os.close();
          }
          catch( Exception e )
          {}
        }
        else
        {
          StringReader sr = new StringReader( request );
          //Debug.level = Debug.ALL;
          //Debug.level = Debug.ERRORS_AND_COMMENTS;
          OGCWebServiceRequest request = CASProtocolFactory.createRequest( id, sr );
          sr.close();

          OGCWebServiceEvent event = new OGCWebServiceEvent_Impl( this, request, null, this );

          // acuire a WCASService from the pool
          service = (OGCWebService)pool.acuireObject();
          service.doService( event );
          int timeLimit = 1000 * 60 * 5;
          long timeStamp = System.currentTimeMillis();
          synchronized( this )
          {
            try
            {
              wait( timeLimit );
            }
            catch( Exception e )
            {}
          }
          timeStamp = System.currentTimeMillis() - timeStamp;
          if( timeStamp > timeLimit )
          {
            StringBuffer sb = new StringBuffer( 200 );
            sb.append( "<OGCWebServiceException>" );
            sb.append( "<Exception><Message>" );
            sb.append( "Request processing exceeds time limit" );
            sb.append( "</Message>" );
            sb.append( "<Locator>WFSServlet:WCAS:perform</Locator>" );
            sb.append( "</Exception></OGCWebServiceException>" );
            write( sb.toString() );
          }

          // release WCASService back to the pool
          pool.releaseObject( service );
        }
      }
      catch( Exception e )
      {
        Debug.debugException( e, null );
      }
    }

    /**
     * @param result
     */
    public synchronized void write( Object result )
    {

      OutputStream os = null;
      try
      {
        os = servletResponse.getOutputStream();
        if( result instanceof String )
        {
          os.write( ( (String)result ).getBytes( "UTF-8" ) );
        }
        else
        {
          OGCWebServiceResponse res = ( (OGCWebServiceEvent)result ).getResponse();

          Document doc = res.getException();
          if( doc != null )
          {
            String s = DOMPrinter.nodeToString( doc, "UTF-8" );
            Writer osw = new BufferedWriter( new OutputStreamWriter( os, "UTF-8" ) );
            osw.write( s );
          }
          else if( res instanceof CASGetRecordResponse )
          {
            CASGetRecordResponse resp = (CASGetRecordResponse)res;
            result = resp.getResponse();
            if( result instanceof String )
            {
              os.write( ( (String)result ).getBytes( "UTF-8" ) );
            }
            else if( result instanceof Document )
            {
              String s = DOMPrinter.nodeToString( (Document)result, "UTF-8" );
              os.write( s.getBytes( "UTF-8" ) );
            }
          }
          else if( res instanceof CASTransactionResponse )
          {
            //CASTransactionResponse resp =
            // (CASTransactionResponse)res;

            // TODO: change against real implementation
            String dummyResponse = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + "<WCAS_Response>"
                + "SUCCESS" + "</WCAS_Response>";
            os.write( dummyResponse.getBytes( "UTF-8" ) );
          }
        }
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
      finally
      {
        if( os != null )
        {
          try
          {
            os.close();
          }
          catch( IOException e )
          {
            e.printStackTrace();
          }
        }
      }

      try
      {
        this.notifyAll();
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
      Thread.currentThread().interrupt();
    }
  }

  /**
   * 
   * 
   * @version $Revision$
   * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
   */
  private class Reloader extends Thread
  {
    /**
     *  
     */
    public void run()
    {
      while( true )
      {
        try
        {
          sleep( 1000 * freq );

          File file = new File( url.getFile() );

          if( file.lastModified() != capaTimestamp )
          {
            System.out.println( "configuration changed: reload WCAS ... " );
            capaTimestamp = file.lastModified();
            initWCAS();
          }
        }
        catch( Exception e )
        {
          System.out.println( e );
        }
      }
    }
  }
}