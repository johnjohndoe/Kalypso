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
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.net.URL;
import java.util.Enumeration;
import java.util.HashMap;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.deegree.model.coverage.CVDescriptor;
import org.deegree.model.coverage.CoverageLayer;
import org.deegree.services.OGCWebService;
import org.deegree.services.OGCWebServiceClient;
import org.deegree.services.OGCWebServiceEvent;
import org.deegree.services.OGCWebServiceException;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.wcs.capabilities.WCSCapabilities;
import org.deegree.services.wcs.protocol.WCSGetCoverageResponse;
import org.deegree.xml.DOMPrinter;
import org.deegree.xml.Marshallable;
import org.deegree_impl.model.cv.CVDescriptorDirFactory;
import org.deegree_impl.model.cv.CVDescriptorFactory;
import org.deegree_impl.model.cv.InvalidAxisDefinitionException;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.services.OGCWebServiceEvent_Impl;
import org.deegree_impl.services.OGCWebServiceException_Impl;
import org.deegree_impl.services.wcs.capabilities.WCSCapabilitiesFactory;
import org.deegree_impl.services.wcs.protocol.WCSProtocolFactory;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.StringExtend;
import org.xml.sax.SAXException;

/**
 * 
 * <p>
 * --------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class WCSServlet extends AbstractOGCServlet
{

  protected String capabilitiesFile = null;

  protected WCServicePool pool = null;

  private CVDescriptor[] descriptors = new CVDescriptor[0];

  private int initInstances = 1;

  private int maxInstances = 30;

  private Exception initException = null;

  protected URL url = null;

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
    enableReloader();
  }

  protected void initService()
  {
    Debug.debugMethodBegin();

    initException = null;

    Debug.setLevel( getInitParameter( "debug" ) );

    // get number of initial WFS instances to create. if no valid value can
    // be read 1 will be used as default
    try
    {
      initInstances = Integer.parseInt( getInitParameter( "initInstances" ) );
    }
    catch( Exception e )
    {}

    // get maximal size of the WFS pool
    // if no max size of the pool is defined 30 will be used as default
    try
    {
      maxInstances = Integer.parseInt( getInitParameter( "maxInstances" ) );
    }
    catch( Exception ex )
    {}

    // read/create capabilities
    capabilitiesFile = getInitParameter( "capabilities" );

    try
    {
      url = new URL( capabilitiesFile );
    }
    catch( Exception e )
    {
      getServletContext().log( e.toString() );
      initException = e;
      return;
    }

    try
    {
      initDescriptors();
      initPool();
    }
    catch( Exception e )
    {
      initException = e;
    }

    Debug.debugMethodEnd();
  }

  /**
   * initializes a descriptor object for each coverage layer defined at the WCS
   * capabilities
   */
  private void initDescriptors() throws IOException, SAXException, InvalidAxisDefinitionException,
      Exception
  {
    Debug.debugMethodBegin();

    URL curl = new URL( capabilitiesFile );
    WCSCapabilities capa = WCSCapabilitiesFactory.createCapabilities( curl );

    CoverageLayer[] cvl = capa.getCoverageLayerList();

    CVDescriptor[] desc = new CVDescriptor[cvl.length];

    for( int i = 0; i < cvl.length; i++ )
    {
      boolean exists = false;
      CVDescriptor d = null;

      for( int j = 0; j < descriptors.length; j++ )
      {
        if( descriptors[j].getCoverageLayer().getLayerID().equals( cvl[i].getLayerID() ) )
        {
          exists = true;
          d = descriptors[j];
          break;
        }
      }

      if( !exists )
      {
        URL url = cvl[i].getDescriptorResource();

        if( cvl[i].getLayerID().startsWith( "DD_" ) )
        {
          desc[i] = CVDescriptorDirFactory.createCVDescriptor( url );
        }
        else
        {
          desc[i] = CVDescriptorFactory.createCVDescriptor( url );
        }
      }
      else
      {
        desc[i] = d;
      }
    }

    descriptors = desc;

    Debug.debugMethodEnd();
  }

  /**
   * initializes the WCService pool
   */
  private void initPool()
  {
    if( pool == null )
    {
      pool = WCServicePool.getInstance( descriptors );
    }
    else
    {
      synchronized( pool )
      {
        pool.destroy();
        pool = WCServicePool.getInstance( descriptors );
      }
    }

    pool.setMaxInstances( maxInstances );

    // initialize XX WCServices to the pool
    pool.fill( initInstances );
  }

  /**
   * performs a http-post request
   */
  public void doPost( HttpServletRequest request, HttpServletResponse response )
      throws ServletException, IOException
  {
    throw new ServletException( "HTTP Post isn't supported yet!" );
  }

  /**
   */
  public void doGet( HttpServletRequest request, HttpServletResponse response )
      throws ServletException, IOException
  {
    Debug.debugMethodBegin();

    if( initException != null )
    {
      handleError( initException, response );
    }
    else
    {
      HashMap model = toModel( request );

      WCS wcs = new WCS( response, model );
      wcs.perform();
    }

    Debug.debugMethodEnd();
  }

  /**
   * 
   * 
   * @param request
   * 
   * @return
   */
  private HashMap toModel( HttpServletRequest request )
  {
    HashMap param = new HashMap();

    Enumeration enum = request.getParameterNames();

    while( enum.hasMoreElements() )
    {
      String name = (String)enum.nextElement();
      Object value = request.getParameter( name );
      name = name.toUpperCase();

      if( name.equals( "RANGE" ) )
      {
        value = StringExtend.toArray( (String)value, ",", false );
      }
      else if( name.equals( "BBOX" ) )
      {
        String[] s = StringExtend.toArray( (String)value, ",", false );
        double minx = Double.parseDouble( s[0] );
        double miny = Double.parseDouble( s[1] );
        double maxx = Double.parseDouble( s[2] );
        double maxy = Double.parseDouble( s[3] );
        value = GeometryFactory.createGM_Envelope( minx, miny, maxx, maxy );
      }
      else if( name.equals( "ELEVATION" ) )
      {
        //TODO
      }
      else if( name.equals( "TIMESTAMP" ) )
      {
        //TODO
      }
      else if( name.equals( "PARAM" ) )
      {
        //TODO
      }
      else if( name.equals( "WIDTH" ) || name.equals( "HEIGHT" ) || name.equals( "DEPTH" ) )
      {
        value = new Integer( (String)value );
      }
      else if( name.equals( "RESX" ) || name.equals( "RESY" ) || name.equals( "RESZ" ) )
      {
        value = new Double( (String)value );
      }

      param.put( name, value );
    }

    return param;
  }

  //////////////////////////////////////////////////////////////////////////
  //                           inner classes //
  //////////////////////////////////////////////////////////////////////////

  /**
   * private inner class for performing the request against the WCService
   */
  private class WCS implements OGCWebServiceClient
  {
    private HashMap model = null;

    private HttpServletResponse response = null;

    private OGCWebService service = null;

    /**
     * Creates a new WCS object.
     * 
     * @param response
     * @param model
     */
    WCS( HttpServletResponse response, HashMap model )
    {
      this.response = response;
      this.model = model;

      String id = "id-" + Math.random();
      model.put( "ID", id );
    }

    /**
     *  
     */
    public void perform()
    {
      Debug.debugMethodBegin();

      try
      {
        String req = (String)model.get( "REQUEST" );
        String srv = (String)model.get( "SERVICE" );
        String version = (String)model.get( "VERSION" );
        if( version.length() < 5 )
        {
          version += ".0";
        }

        // is correct service requested
        if( !"WCS".equals( srv ) )
        {
          OGCWebServiceException oe = new OGCWebServiceException_Impl( "WCS",
              "wrong service requested" + "must be WCS" );
          response.setContentType( "application/vnd.ogc.se_xml" );

          PrintWriter pw = response.getWriter();
          pw.print( ( (Marshallable)oe ).exportAsXML() );
          pw.close();
          return;
        }

        // is requested service version valid
        if( ( version == null ) || ( version.compareTo( "0.7.0" ) < 0 )
            || ( version.compareTo( "1.0.0" ) > 0 ) )
        {
          OGCWebServiceException oe = new OGCWebServiceException_Impl( "WCS",
              "not supported service version " + "must be between 0.7.0 and 1.0.0" );
          response.setContentType( "application/vnd.ogc.se_xml" );

          PrintWriter pw = response.getWriter();
          pw.print( ( (Marshallable)oe ).exportAsXML() );
          pw.close();
          return;
        }

        OGCWebServiceRequest re = null;

        if( "GetCoverage".equals( req ) )
        {
          re = WCSProtocolFactory.createWCSGetCoverageRequest( model );
        }
        else if( "GetCapabilities".equals( req ) )
        {
          URL url = new URL( capabilitiesFile );
          BufferedReader br = new BufferedReader( new InputStreamReader( url.openStream() ) );
          StringBuffer sb = new StringBuffer( 10000 );
          String line = null;

          while( ( line = br.readLine() ) != null )
          {
            sb.append( line );
          }

          br.close();
          response.setContentType( "text/xml" );

          PrintWriter pw = response.getWriter();
          pw.print( sb.toString() );
          pw.close();
          return;
        }
        else if( "WCSDescribeCoverageLayer".equals( req ) )
        {
          OGCWebServiceException oe = new OGCWebServiceException_Impl( "WCS",
              "WCSDescribeCoverageLayer is " + "not supported yet" );
          response.setContentType( "application/vnd.ogc.se_xml" );

          PrintWriter pw = response.getWriter();
          pw.print( ( (Marshallable)oe ).exportAsXML() );
          pw.close();
          return;
        }
        else
        {
          OGCWebServiceException oe = new OGCWebServiceException_Impl( "WCS", "not supported "
              + "request: " + req );
          response.setContentType( "application/vnd.ogc.se_xml" );

          PrintWriter pw = response.getWriter();
          pw.print( ( (Marshallable)oe ).exportAsXML() );
          pw.close();
          return;
        }

        OGCWebServiceEvent event = new OGCWebServiceEvent_Impl( this, re, null, this );

        // acuire a WCService from the pool
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
          sb.append( "<Locator>WFSServlet:WCS:perform</Locator>" );
          sb.append( "</Exception></OGCWebServiceException>" );
          write( sb.toString() );
        }
      }
      catch( Exception e )
      {
        System.out.println( e );

        try
        {
          OGCWebServiceException oe = new OGCWebServiceException_Impl( "WCS", e.toString() );
          response.setContentType( "application/vnd.ogc.se_xml" );

          PrintWriter pw = response.getWriter();
          pw.print( ( (Marshallable)oe ).exportAsXML() );
          pw.close();
        }
        catch( Exception ee )
        {
          System.out.println( ee );
        }
      }

      Debug.debugMethodEnd();
    }

    /**
     * recieves the response from the WCService and sends it to the requesting
     * client.
     */
    public synchronized void write( Object object )
    {
      Debug.debugMethodBegin();

      try
      {
        pool.releaseObject( service );

        OGCWebServiceEvent event = (OGCWebServiceEvent)object;
        object = event.getResponse();

        OutputStream os = null;

        // TODO mime type
        if( object instanceof String )
        {
          os = response.getOutputStream();
          os.write( object.toString().getBytes() );
        }
        else if( !( object instanceof WCSGetCoverageResponse )
            && !( object instanceof WCSCapabilities ) )
        {
          String s = "The recieved response isn't a valid WCS response object";
          os = response.getOutputStream();
          os.write( s.getBytes() );
        }
        else
        {
          WCSGetCoverageResponse res = (WCSGetCoverageResponse)object;

          if( res.getException() != null )
          {
            String s = DOMPrinter.nodeToString( res.getException(), "UTF-8" );
            os = response.getOutputStream();
            os.write( s.getBytes() );
          }
          else
          {
            os = response.getOutputStream();
            os.write( (byte[])res.getResponse() );
          }
        }

        os.close();
      }
      catch( IOException ioe )
      {
        System.out.println( "fatal error IOException:\n" + ioe );
      }
      catch( Exception e )
      {
        System.out.println( "fatal error\n" + e );
      }

      try
      {
        this.notifyAll();
      }
      catch( Exception e )
      {}
      Thread.currentThread().interrupt();

      Debug.debugMethodEnd();
    }

  }

}