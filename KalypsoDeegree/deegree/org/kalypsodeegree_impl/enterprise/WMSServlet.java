// $Header:
// /cvsroot/deegree/deegree/org/deegree_impl/enterprise/WMSServlet.java,v 1.57
// 2004/08/20 08:43:18 poth Exp $

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

import java.awt.Color;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.deegree.graphics.Encoders;
import org.deegree.services.InconsistentRequestException;
import org.deegree.services.OGCWebService;
import org.deegree.services.OGCWebServiceClient;
import org.deegree.services.OGCWebServiceEvent;
import org.deegree.services.OGCWebServiceException;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.OGCWebServiceResponse;
import org.deegree.services.wms.CurrentUpdateSequenceException;
import org.deegree.services.wms.InvalidFormatException;
import org.deegree.services.wms.InvalidSRSException;
import org.deegree.services.wms.InvalidUpdateSequenceException;
import org.deegree.services.wms.LayerNotDefinedException;
import org.deegree.services.wms.LayerNotQueryableException;
import org.deegree.services.wms.ServiceExceptionReport;
import org.deegree.services.wms.StyleNotDefinedException;
import org.deegree.services.wms.capabilities.Format;
import org.deegree.services.wms.capabilities.Operation;
import org.deegree.services.wms.capabilities.Request;
import org.deegree.services.wms.capabilities.WMSCapabilities;
import org.deegree.services.wms.protocol.WMSDescribeLayerResponse;
import org.deegree.services.wms.protocol.WMSGetCapabilitiesResponse;
import org.deegree.services.wms.protocol.WMSGetFeatureInfoRequest;
import org.deegree.services.wms.protocol.WMSGetFeatureInfoResponse;
import org.deegree.services.wms.protocol.WMSGetLegendGraphicRequest;
import org.deegree.services.wms.protocol.WMSGetLegendGraphicResponse;
import org.deegree.services.wms.protocol.WMSGetMapRequest;
import org.deegree.services.wms.protocol.WMSGetMapResponse;
import org.deegree.services.wms.protocol.WMSGetScaleBarRequest;
import org.deegree.services.wms.protocol.WMSGetScaleBarResponse;
import org.deegree.services.wms.protocol.WMSGetStylesResponse;
import org.deegree.services.wms.protocol.WMSPutStylesResponse;
import org.deegree.xml.DOMPrinter;
import org.deegree.xml.Marshallable;
import org.deegree.xml.XMLParsingException;
import org.deegree.xml.XMLTools;
import org.deegree_impl.services.OGCWebServiceEvent_Impl;
import org.deegree_impl.services.OGCWebServiceException_Impl;
import org.deegree_impl.services.wms.ServiceExceptionReport_Impl;
import org.deegree_impl.services.wms.capabilities.DeegreeWMSCapabilitiesFactory;
import org.deegree_impl.services.wms.protocol.WMSProtocolFactory;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.IDGenerator;
import org.deegree_impl.tools.MimeTypeMapper;
import org.deegree_impl.tools.NetWorker;
import org.deegree_impl.tools.StringExtend;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * /** Serves as an OCC-compliant HTTP-frontend to the WMS.
 * <p>
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @author last edited by: $Author$
 * 
 * @version 1.0. $Revision$, $Date$
 * 
 * @since 1.0
 */
public class WMSServlet extends AbstractOGCServlet
{
  protected WMSCapabilities wmsCapabilities = null;

  // Pool of instances of the underlying WMS-Service.
  protected WMServicePool servicePool;

  private Exception initException = null;

  /**
   * Called by the server (via the service method) to allow a servlet to handle
   * a GET request.
   * 
   * @param req
   *          an HttpServletRequest object that contains the request the client
   *          has made to the servlet
   * @param resp
   *          an HttpServletResponse object that contains the response the
   *          servlet sends to the client
   */
  public void doGet( HttpServletRequest req, HttpServletResponse resp )
  {
    Debug.debugMethodBegin( this, "doGet" );

    if( initException != null )
    {
      handleError( initException, resp );
    }
    else
    {
      new WMS( getParamMap( req ), resp );
    }

    Debug.debugMethodEnd();
  }

  /**
   * 
   * 
   * @param req
   * @param resp
   */
  public void doPost( HttpServletRequest req, HttpServletResponse resp )
  {
    Debug.debugMethodBegin();

    if( initException != null )
    {
      handleError( initException, resp );
    }
    else
    {
      new WMS( req, resp );
    }

    Debug.debugMethodEnd();
  }

  /**
   * Called by the servlet container to indicate that the servlet is being
   * placed into service.
   * 
   * @param servletConfig
   *          servlet capabilities
   * @throws ServletException
   *           exception if something occurred that interferes with the
   *           servlet's normal operation
   */
  public void init( ServletConfig servletConfig ) throws ServletException
  {
    super.init( servletConfig );
    enableReloader();
  }

  /**
   * Called when reinitialization of the service is necessary, e.g. the
   * capabilities file has been altered.
   */
  protected void initService()
  {
    initException = null;
    String capabilities = getInitParameter( "capabilities" );

    if( capabilities == null )
    {
      getServletContext().log( "Parameter 'capabilities' unspecified. Exiting." );
      initException = new Exception( "Parameter 'capabilities' unspecified. Exiting." );
      return;
    }

    try
    {
      capabilitiesURL = new URL( capabilities );
    }
    catch( MalformedURLException e )
    {
      getServletContext().log( e.toString() );
      initException = e;
      return;
    }

    try
    {
      DeegreeWMSCapabilitiesFactory fac = new DeegreeWMSCapabilitiesFactory();
      wmsCapabilities = fac.createCapabilities( capabilitiesURL );

      // get the service-pool for WMS services
      if( servicePool == null )
      {
        servicePool = WMServicePool.getInstance( wmsCapabilities );
      }
      else
      {
        // clear, destroy and re-initialize the service-pool if the
        // capabilities file of the WMS has been changed
        synchronized( servicePool )
        {
          servicePool.destroy();
          servicePool = WMServicePool.getInstance( wmsCapabilities );
        }
      }

      // evaluate 'maxInstances' parameter
      try
      {
        String maxInstancesStr = getInitParameter( "maxInstances" );

        if( maxInstancesStr == null )
        {
          getServletContext().log(
              "Parameter 'maxInstances' " + "unspecified. Using default value." );
        }
        else
        {
          servicePool.setMaxInstances( Integer.parseInt( maxInstancesStr ) );
        }
      }
      catch( NumberFormatException e )
      {
        getServletContext().log(
            "Parameter 'maxInstances' has " + "invalid format. Using default value." );
      }

      // evaluate 'initInstances' parameter
      try
      {
        String initInstancesStr = getInitParameter( "initInstances" );

        if( initInstancesStr == null )
        {
          getServletContext().log(
              "Parameter 'initInstances' " + "unspecified. Using default value." );
        }
        else
        {
          servicePool.fill( Integer.parseInt( initInstancesStr ) );
        }
      }
      catch( NumberFormatException e )
      {
        getServletContext().log(
            "Parameter 'initInstances' has invalid " + "format. Using default value." );
      }
    }
    catch( Exception e )
    {
      getServletContext().log( e.toString() );
      initException = e;
    }
  }

  //////////////////////////////////////////////////////////////////////////
  //                           inner classes //
  //////////////////////////////////////////////////////////////////////////

  /**
   * 
   * 
   * @version $Revision$
   * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
   */
  private class WMS implements OGCWebServiceClient
  {
    private Color bgColor = Color.WHITE;

    private HttpServletResponse resp = null;

    private OGCWebServiceRequest request = null;

    private String exceptionFormat = "application/vnd.ogc.se_xml";

    private String format = null;

    private boolean transparent = false;

    private int height = 400;

    private int width = 600;

    /**
     * Constructor of the inner class WMS. The constructor accesses a
     * <tt>WMService</tt> instance from the <tt>WMServicePool</tt> an calls
     * it doService(..) method.
     * <p>
     * If the request can't be performed an exception will be written to the
     * output stream to the client. The exception format is taken from the
     * request (default: application/vnd.ogc.se_xml). This constructor will be
     * called if the request is received by the servlet with HTTP POST
     */
    private WMS( HttpServletRequest srequest, HttpServletResponse resp )
    {

      Debug.debugMethodBegin();
      this.resp = resp;

      String service = null;
      Document doc = null;
      try
      {
        String content = getPostContent( srequest );
        doc = XMLTools.parse( new StringReader( content ) );
        service = XMLTools.getRequiredAttrValue( "service", doc.getDocumentElement() );
      }
      catch( Exception e )
      {
        writeException( "WMS:WMS", "could not parse incomming post-requestn"
            + StringExtend.stackTraceToString( e.getStackTrace() ) );
        return;
      }

      if( !( "WMS".equalsIgnoreCase( service ) ) )
      {
        writeException( "WMS:WMS", "wrong service requested: " + service );
        return;
      }

      // create request object
      try
      {
        long id = IDGenerator.getInstance().generateUniqueID();
        request = WMSProtocolFactory.createGetMapRequest( "id-" + id, doc.getDocumentElement() );
      }
      catch( MalformedURLException ex )
      {
        writeException( "WMS:WMS", "MalformedURLException "
            + "for creating Request in WMSServlet: " + ex.getMessage() );
        return;
      }
      catch( XMLParsingException pe )
      {
        writeException( "WMS:WMS XMLParsingException", pe.toString() );
        return;
      }
      catch( InvalidFormatException ie )
      {
        writeServiceExceptionReport( "InvalidFormat", ie.toString() );
        return;
      }
      catch( InconsistentRequestException ie )
      {
        writeException( "WMS:WMS InconsistentRequestException", ie.toString() );
        return;
      }

      performRequest( request );

      Debug.debugMethodEnd();
    }

    /**
     * Constructor of the inner class WMS. The constructor accesses a
     * <tt>WMService</tt> instance from the <tt>WMServicePool</tt> an calls
     * it doService(..) method.
     * <p>
     * If the request can't be performed an exception will be written to the
     * output stream to the client. The exception format is taken from the
     * request (default: application/vnd.ogc.se_xml). This constructor will be
     * called if the request is received by the servlet with HTTP GET
     */
    private WMS( HashMap paramMap, HttpServletResponse resp )
    {
      this.resp = resp;

      // get essential parameters to handle exceptions
      if( paramMap.get( "EXCEPTIONS" ) != null )
      {
        exceptionFormat = (String)paramMap.get( "EXCEPTIONS" );
      }

      if( paramMap.get( "WIDTH" ) != null )
      {
        width = Integer.parseInt( (String)paramMap.get( "WIDTH" ) );
      }

      if( paramMap.get( "HEIGHT" ) != null )
      {
        height = Integer.parseInt( (String)paramMap.get( "HEIGHT" ) );
      }

      if( paramMap.get( "BGCOLOR" ) != null )
      {
        try
        {
          bgColor = Color.decode( (String)paramMap.get( "BGCOLOR" ) );
        }
        catch( Exception e )
        {}
      }

      format = (String)paramMap.get( "FORMAT" );

      if( paramMap.get( "TRANSPARENT" ) != null )
      {
        String s = (String)paramMap.get( "TRANSPARENT" );
        transparent = "TRUE".equalsIgnoreCase( s );
      }

      // check if the right service is requested
      if( paramMap.get( "SERVICE" ) != null )
      {
        String s = (String)paramMap.get( "SERVICE" );

        if( !( "WMS".equalsIgnoreCase( s ) ) )
        {
          writeException( "WMS:WMS", "wrong service requested: " + s );
          return;
        }
      }

      // create request object
      try
      {
        long id = IDGenerator.getInstance().generateUniqueID();

        // add capabilites to the parameter list so the protocol factory
        // is able to consider them if required
        paramMap.put( "CAPABILITIES", wmsCapabilities );
        request = WMSProtocolFactory.createRequest( "id-" + id, paramMap );
      }
      catch( MalformedURLException ex )
      {
        writeException( "WMS:WMS", "MalformedURLException "
            + "for creating Request in WMSServlet: " + ex.getMessage() );
        return;
      }
      catch( XMLParsingException pe )
      {
        writeException( "WMS:WMS XMLParsingException", pe.toString() );
        return;
      }
      catch( InvalidFormatException ie )
      {
        writeServiceExceptionReport( "InvalidFormat", ie.toString() );
        return;
      }
      catch( InconsistentRequestException ie )
      {
        writeException( "WMS:WMS InconsistentRequestException", ie.toString() );
        return;
      }

      performRequest( request );
    }

    /**
     * performs the passed OGCWebServiceRequest by accessing service from the
     * pool and passing the request to it
     */
    private void performRequest( OGCWebServiceRequest request )
    {
      OGCWebServiceEvent event = new OGCWebServiceEvent_Impl( this, request, null, this );

      OGCWebService service = null;

      try
      {
        // acuire a WMService from the pool
        service = (OGCWebService)servicePool.acuireObject();

        if( service == null )
        {
          writeException( "WMS:WMS", "could not access a WMService instance" );
          return;
        }

        service.doService( event );
        int timeLimit = wmsCapabilities.getDeegreeParam().getRequestTimeLimit() * 1000;
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
          writeException( "WMS:WMS Exception", "Request performing exceeds time limit." );
        }
      }
      catch( InvalidUpdateSequenceException iuse )
      {
        writeServiceExceptionReport( "InvalidUpdateSequence", iuse.toString() );
      }
      catch( InvalidSRSException isrse )
      {
        writeServiceExceptionReport( "InvalidSRS", isrse.toString() );
      }
      catch( CurrentUpdateSequenceException cuse )
      {
        writeServiceExceptionReport( "CurrentUpdateSequence", cuse.toString() );
      }
      catch( LayerNotDefinedException lnde )
      {
        writeServiceExceptionReport( "LayerNotDefined", lnde.toString() );
      }
      catch( LayerNotQueryableException lnqe )
      {
        writeServiceExceptionReport( "LayerNotQueryable", lnqe.toString() );
      }
      catch( StyleNotDefinedException snde )
      {
        writeServiceExceptionReport( "StyleNotDefined", snde.toString() );
      }
      catch( InvalidFormatException ife )
      {
        System.out.println( "InvalidFormatException --- " );
        writeServiceExceptionReport( "InvalidFormat", ife.toString() );
      }
      catch( Exception e )
      {
        e.printStackTrace();
        writeException( "WMS:WMS Exception", e.toString() );
      }

      // release service back to the pool
      try
      {
        servicePool.releaseObject( service );
      }
      catch( Exception e )
      {
        e.printStackTrace();
        Debug.debugException( e, "" );
      }
    }

    /**
     * returns the content of the http post request without its header
     */
    private String getPostContent( HttpServletRequest request ) throws IllegalArgumentException,
        IOException
    {
      Debug.debugMethodBegin();

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
     * 
     * 
     * @param result
     */
    public synchronized void write( Object result )
    {
      Debug.debugMethodBegin( this, "write" );

      try
      {
        OGCWebServiceEvent event = (OGCWebServiceEvent)result;
        OGCWebServiceResponse response = event.getResponse();

        if( response.getException() != null )
        {
          // handle the case that an exception occured during the
          // request performance
          Document doc = response.getException();
          String l = "";
          String m = "";
          NodeList nl = doc.getElementsByTagName( "Locator" );

          if( nl.getLength() > 0 )
          {
            l = XMLTools.getStringValue( nl.item( 0 ) );
          }

          nl = doc.getElementsByTagName( "Message" );

          if( nl.getLength() > 0 )
          {
            m = XMLTools.getStringValue( nl.item( 0 ) );
          }

          writeException( l, m );
        }
        else
        {
          if( response instanceof WMSGetCapabilitiesResponse )
          {
            handleGetCapabilitiesResponse( (WMSGetCapabilitiesResponse)response );
          }
          else if( response instanceof WMSGetMapResponse )
          {
            handleGetMapResponse( (WMSGetMapResponse)response );
          }
          else if( response instanceof WMSGetFeatureInfoResponse )
          {
            handleFeatureInfoResponse( (WMSGetFeatureInfoResponse)response );
          }
          else if( response instanceof WMSGetStylesResponse )
          {
            handleGetStylesResponse( (WMSGetStylesResponse)response );
          }
          else if( response instanceof WMSPutStylesResponse )
          {
            handlePutStylesResponse( (WMSPutStylesResponse)response );
          }
          else if( response instanceof WMSDescribeLayerResponse )
          {
            handleDescribeLayerResponse( (WMSDescribeLayerResponse)response );
          }
          else if( response instanceof WMSGetScaleBarResponse )
          {
            handleGetScaleBarResponse( (WMSGetScaleBarResponse)response );
          }
          else if( response instanceof WMSGetLegendGraphicResponse )
          {
            handleGetLegendGraphicResponse( (WMSGetLegendGraphicResponse)response );
          }
        }
      }
      catch( InvalidFormatException ife )
      {
        Debug.debugException( ife, "-" );
        writeServiceExceptionReport( "InvalidFormat", ife.toString() );
      }
      catch( Exception e )
      {
        Debug.debugException( e, "-" );
        writeException( "WMS:write", e.toString() );
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

    /**
     * handles the response to a get capabilities request
     * 
     * @param response
     */
    private void handleGetCapabilitiesResponse( WMSGetCapabilitiesResponse response )
        throws Exception
    {
      Debug.debugMethodBegin( this, "handleGetCapabilitiesResponse" );
      resp.setContentType( "application/vnd.ogc.wms_xml" );
      WMSCapabilities capa = response.getCapabilities();
      String rd = wmsCapabilities.getDeegreeParam().getRootDirectory();
      URL url = new URL( "file:///" + rd + "/WEB-INF/xml/toWMS111Capabilities.xsl" );
      String xml = xsltTransformGetFeature( capa.exportAsXML(), url );
      String dtd = NetWorker.url2String( wmsCapabilities.getDeegreeParam().getDTDLocation() );
      StringBuffer sb = new StringBuffer();
      sb.append( "<!DOCTYPE WMT_MS_Capabilities SYSTEM " );
      sb.append( "'" + dtd + "' \n" );
      sb.append( "[\n<!ELEMENT VendorSpecificCapabilities EMPTY>\n]>" );

      xml = StringExtend.replace( xml, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
          "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + "\n" + sb.toString(), false );
      xml = StringExtend.replace( xml, "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"",
          "", false );

      try
      {
        PrintWriter pw = resp.getWriter();
        pw.print( xml );
        pw.close();
      }
      catch( Exception e )
      {
        Debug.debugException( e, "-" );
      }

      Debug.debugMethodEnd();
    }

    /**
     * handles the response to a get map request
     * 
     * @param response
     */
    private void handleGetMapResponse( WMSGetMapResponse response ) throws InvalidFormatException
    {
      Debug.debugMethodBegin();

      String mime = MimeTypeMapper.toMimeType( ( (WMSGetMapRequest)request ).getFormat() );

      if( !MimeTypeMapper.isImageType( mime ) )
      {
        throw new InvalidFormatException( mime + " is not a known image format" );
      }

      writeImage( response.getMap(), mime );

      Debug.debugMethodEnd();
    }

    /**
     * handles the response to a get featureinfo request
     * 
     * @param response
     */
    private void handleFeatureInfoResponse( WMSGetFeatureInfoResponse response ) throws Exception
    {
      Debug.debugMethodBegin();

      String s = ( (WMSGetFeatureInfoRequest)request ).getInfoFormat();
      String mime = MimeTypeMapper.toMimeType( s );
      resp.setContentType( mime );

      String fir = response.getFeatureInfo();

      Request re = wmsCapabilities.getCapability().getRequest();
      Operation operation = re.getOperation( Operation.GETFEATUREINFO );
      Format format = operation.getFormat( s );
      URL url = format.getFilter();

      if( url != null )
      {
        fir = xsltTransformGetFeature( fir, url );
      }

      try
      {
        OutputStreamWriter os = new OutputStreamWriter( resp.getOutputStream(), "UTF-8" );
        os.write( fir );
        os.close();
      }
      catch( Exception e )
      {
        System.out.println( e );
      }

      Debug.debugMethodEnd();
    }

    /**
     * handles the response to a get styles request
     * 
     * @param response
     */
    private void handleGetStylesResponse( WMSGetStylesResponse response )
    {
      Debug.debugMethodBegin();
      Debug.debugMethodEnd();
    }

    /**
     * handles the response to a put styles request
     * 
     * @param response
     */
    private void handlePutStylesResponse( WMSPutStylesResponse response )
    {
      Debug.debugMethodBegin( this, "handleFeatureInfoResponse" );
      Debug.debugMethodEnd();
    }

    /**
     * handles the response to a describe layer request
     * 
     * @param response
     */
    private void handleDescribeLayerResponse( WMSDescribeLayerResponse response )
    {
      Debug.debugMethodBegin( this, "handleDescribeLayerResponse" );
      Debug.debugMethodEnd();
    }

    /**
     * handles the response to a get legend graphic request
     * 
     * @param response
     */
    private void handleGetLegendGraphicResponse( WMSGetLegendGraphicResponse response )
        throws Exception
    {
      Debug.debugMethodBegin();

      String mime = MimeTypeMapper.toMimeType( ( (WMSGetLegendGraphicRequest)request ).getFormat() );

      if( !MimeTypeMapper.isImageType( mime ) )
      {
        throw new InvalidFormatException( mime + " is not a known image format" );
      }

      writeImage( response.getLegendGraphic(), mime );

      Debug.debugMethodEnd();
    }

    /**
     * handles the response to a get scalebar request
     * 
     * @param response
     */
    private void handleGetScaleBarResponse( WMSGetScaleBarResponse response ) throws Exception
    {
      Debug.debugMethodBegin();

      String mime = MimeTypeMapper.toMimeType( ( (WMSGetScaleBarRequest)request ).getFormat() );

      if( !MimeTypeMapper.isImageType( mime ) )
      {
        throw new InvalidFormatException( mime + " is not a known image format" );
      }

      writeImage( response.getScaleBar(), mime );

      Debug.debugMethodEnd();
    }

    /**
     * writes an excetion into the <tt>OutputStream</tt> back to the client.
     * the method considers the format an exception shall be returned to the
     * client as defined in the request.
     * 
     * @param location
     *          location where the exception occurs
     * @param message
     *          message describing the exception
     */
    public void writeException( String location, String message )
    {
      Debug.debugMethodBegin( this, "writeException" );

      if( exceptionFormat.equals( "application/vnd.ogc.se_inimage" ) )
      {
        BufferedImage bi = new BufferedImage( width, height, BufferedImage.TYPE_INT_ARGB );
        Graphics g = bi.getGraphics();
        g.setColor( Color.BLUE );
        g.drawString( location, 5, 20 );
        int pos1 = message.indexOf( ':' );
        g.drawString( message.substring( 0, pos1 + 1 ), 5, 50 );
        g.drawString( message.substring( pos1 + 1, message.length() ), 5, 80 );
        String mime = MimeTypeMapper.toMimeType( format );
        writeImage( bi, mime );
      }
      else if( exceptionFormat.equals( "application/vnd.ogc.se_blank" ) )
      {
        BufferedImage bi = new BufferedImage( width, height, BufferedImage.TYPE_INT_ARGB );
        Graphics g = bi.getGraphics();
        g.setColor( bgColor );
        g.fillRect( 0, 0, bi.getWidth(), bi.getHeight() );
        g.dispose();
        String mime = MimeTypeMapper.toMimeType( format );
        writeImage( bi, mime );
      }
      else
      {
        // default --> application/vnd.ogc.se_xml
        OGCWebServiceException exce = new OGCWebServiceException_Impl( location, message );

        try
        {
          resp.setContentType( "application/vnd.ogc.se_xml" );
          PrintWriter pw = resp.getWriter();
          pw.print( ( (Marshallable)exce ).exportAsXML() );
          pw.close();
        }
        catch( Exception e )
        {
          Debug.debugException( e, "-" );
        }
      }

      Debug.debugMethodEnd();
    }

    /**
     * writes an service exception report into the <tt>OutputStream</tt> back
     * to the client. the method considers the format an exception shall be
     * returned to the client as defined in the request.
     * 
     * @param code
     *          code of te expetion as defined in the OGC WMS 1.1.1 spec and the
     *          WMS 1.1.1 test suite
     * @param report
     *          detailed description of the exption
     */
    public void writeServiceExceptionReport( String code, String report )
    {
      Debug.debugMethodBegin();

      if( exceptionFormat.equals( "application/vnd.ogc.se_inimage" ) )
      {
        BufferedImage bi = new BufferedImage( width, height, BufferedImage.TYPE_INT_ARGB );
        Graphics g = bi.getGraphics();

        if( !transparent )
        {
          g.setColor( bgColor );
          g.fillRect( 0, 0, bi.getWidth(), bi.getHeight() );
        }

        g.setColor( Color.BLUE );
        g.drawString( code, 5, 20 );
        int pos1 = report.indexOf( ':' );
        g.drawString( report.substring( 0, pos1 + 1 ), 5, 50 );
        g.drawString( report.substring( pos1 + 1, report.length() ), 5, 80 );
        String mime = MimeTypeMapper.toMimeType( format );
        writeImage( bi, mime );
      }
      else if( exceptionFormat.equals( "application/vnd.ogc.se_blank" ) )
      {
        BufferedImage bi = new BufferedImage( width, height, BufferedImage.TYPE_INT_ARGB );
        Graphics g = bi.getGraphics();

        if( !transparent )
        {
          g.setColor( bgColor );
          g.fillRect( 0, 0, bi.getWidth(), bi.getHeight() );
        }

        g.dispose();
        String mime = MimeTypeMapper.toMimeType( format );
        writeImage( bi, mime );
      }
      else
      {
        // default --> application/vnd.ogc.se_xml
        ServiceExceptionReport exce = new ServiceExceptionReport_Impl( "1.1.1", code, report );

        try
        {
          resp.setContentType( "application/vnd.ogc.se_xml" );
          PrintWriter pw = resp.getWriter();
          pw.print( ( (Marshallable)exce ).exportAsXML() );
          pw.close();
        }
        catch( Exception e )
        {
          Debug.debugException( e, "-" );
        }
      }

      Debug.debugMethodEnd();
    }

    /**
     * writes the passed image to the response output stream.
     * 
     * @param output
     * @param mime
     */
    private void writeImage( Object output, String mime )
    {
      try
      {
        OutputStream os = null;
        resp.setContentType( mime );

        if( mime.equalsIgnoreCase( "image/gif" ) )
        {
          os = resp.getOutputStream();
          Encoders.encodeGif( os, (BufferedImage)output );
        }
        else if( mime.equalsIgnoreCase( "image/jpg" ) || mime.equalsIgnoreCase( "image/jpeg" ) )
        {
          os = resp.getOutputStream();
          Encoders.encodeJpeg( os, (BufferedImage)output, wmsCapabilities.getDeegreeParam()
              .getMapQuality() );
        }
        else if( mime.equalsIgnoreCase( "image/png" ) )
        {
          os = resp.getOutputStream();
          Encoders.encodePng( os, (BufferedImage)output );
        }
        else if( mime.equalsIgnoreCase( "image/tif" ) || mime.equalsIgnoreCase( "image/tiff" ) )
        {
          os = resp.getOutputStream();
          Encoders.encodeTiff( os, (BufferedImage)output );
        }
        else if( mime.equalsIgnoreCase( "image/bmp" ) )
        {
          os = resp.getOutputStream();
          Encoders.encodeBmp( os, (BufferedImage)output );
        }
        else if( mime.equalsIgnoreCase( "image/svg+xml" )
            || mime.equalsIgnoreCase( "image/svg xml" ) )
        {
          os = resp.getOutputStream();
          PrintWriter pw = new PrintWriter( os );
          DOMPrinter.printNode( pw, (Node)output );
          pw.close();
        }
        else
        {
          resp.setContentType( "text/xml" );
          os = resp.getOutputStream();
          OGCWebServiceException exce = new OGCWebServiceException_Impl( "WMS:writeImage",
              "unsupported image format: " + mime );
          os.write( ( (Marshallable)exce ).exportAsXML().getBytes() );
        }

        os.close();
      }
      catch( Exception e )
      {
        //System.out.println(e);
      }
    }

    /**
     * transforms the configuration/capabilities using to a OGC WMS conform
     * capabilities document using a predefined xslt-stylesheet
     */
    private String xsltTransformGetFeature( String capa, URL xslt )
    {
      Debug.debugMethodBegin();

      String out = null;

      try
      {
        Document document = XMLTools.parse( new StringReader( capa ) );

        // Use the static TransformerFactory.newInstance() method to instantiate
        // a TransformerFactory. The javax.xml.transform.TransformerFactory
        // system property setting determines the actual class to instantiate --
        // org.apache.xalan.transformer.TransformerImpl.
        TransformerFactory tFactory = TransformerFactory.newInstance();

        // Use the TransformerFactory to instantiate a Transformer that will
        // work with
        // the stylesheet you specify. This method call also processes the
        // stylesheet
        // into a compiled Templates object.
        Transformer transformer = tFactory.newTransformer( new StreamSource( xslt.openStream() ) );

        // Use the Transformer to apply the associated Templates object to an
        // XML document
        // (foo.xml) and write the output to a file (foo.out).
        StringWriter sw = new StringWriter();
        transformer.transform( new DOMSource( document ), new StreamResult( sw ) );

        out = sw.toString();
      }
      catch( Exception e )
      {
        Debug.debugException( e, "an error/fault body for the soap message will be created" );
        // TODO: exception
      }

      Debug.debugMethodEnd();
      return out;
    }
  }
}
/*******************************************************************************
 * ****************************************************************************
 * Changes to this class. What the people have been up to: $Log:
 * WMSServlet.java,v $ Revision 1.57 2004/08/20 08:43:18 poth no message
 * 
 * Revision 1.56 2004/06/24 14:20:27 poth no message
 * 
 * Revision 1.55 2004/04/13 11:37:43 poth no message
 * 
 * Revision 1.54 2004/04/05 08:34:44 poth no message
 * 
 * Revision 1.53 2004/04/02 06:41:41 poth no message
 * 
 * Revision 1.52 2004/03/31 15:40:20 poth no message
 * 
 * Revision 1.51 2004/03/12 15:56:47 poth no message
 * 
 * 
 *  
 ******************************************************************************/