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
package org.deegree_impl.services.wfs;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.net.URL;
import java.util.HashMap;

import org.deegree.gml.GMLFeatureCollection;
import org.deegree.services.OGCWebServiceClient;
import org.deegree.services.OGCWebServiceEvent;
import org.deegree.services.OGCWebServiceException;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.OGCWebServiceResponse;
import org.deegree.services.WebServiceException;
import org.deegree.services.capabilities.DCPType;
import org.deegree.services.capabilities.HTTP;
import org.deegree.services.wfs.capabilities.Request;
import org.deegree.services.wfs.capabilities.WFSCapabilities;
import org.deegree.services.wfs.protocol.WFSDescribeFeatureTypeRequest;
import org.deegree.services.wfs.protocol.WFSGetCapabilitiesRequest;
import org.deegree.services.wfs.protocol.WFSGetFeatureRequest;
import org.deegree.services.wfs.protocol.WFSLockFeatureRequest;
import org.deegree.services.wfs.protocol.WFSTransactionRequest;
import org.deegree.xml.Marshallable;
import org.deegree.xml.XMLTools;
import org.deegree_impl.gml.GMLFeatureCollection_Impl;
import org.deegree_impl.services.OGCWebServiceEvent_Impl;
import org.deegree_impl.services.OGCWebServiceException_Impl;
import org.deegree_impl.services.OGCWebService_Impl;
import org.deegree_impl.services.wfs.capabilities.WFSCapabilitiesFactory;
import org.deegree_impl.services.wfs.protocol.WFSProtocolFactory;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.MimeTypeMapper;
import org.deegree_impl.tools.NetWorker;
import org.w3c.dom.Document;

/**
 * An instance of the class acts as a wrapper to a remote WFS.
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class RemoteWFService extends OGCWebService_Impl
{

  protected static final String GETCAPABILITIES = "GETCAPABILITIES";

  protected static final String GETFEATURE = "GETFEATURE";

  protected static final String GETFEATUREWITHLOCK = "GETFEATUREWITHLOCK";

  protected static final String DESCRIBEFEATURETYPE = "DESCRIBEFEATURETYPE";

  protected static final String TRANSACTION = "TRANSACTION";

  protected static final String LOCKFEATURE = "LOCKFEATURE";

  protected HashMap addresses = new HashMap();

  /** Creates a new instance of RemoteWFService */
  public RemoteWFService( WFSCapabilities capabilities ) throws WebServiceException
  {
    this.capabilities = capabilities;

    Request request = capabilities.getCapability().getRequest();

    // get GetCapabilities address
    DCPType[] dcp = request.getGetCapabilities().getDCPType();
    URL[] get = ( (HTTP)dcp[0].getProtocol() ).getGetOnlineResources();
    addresses.put( GETCAPABILITIES, get[0] );

    // get GetFeature address
    dcp = request.getGetFeature().getDCPType();
    boolean po = false;
    for( int i = 0; i < dcp.length; i++ )
    {
      get = ( (HTTP)dcp[i].getProtocol() ).getPostOnlineResources();
      if( get != null && get.length > 0 )
      {
        addresses.put( GETFEATURE, get[0] );
        po = true;
      }
    }
    if( !po )
    {
      String s = "WFS: " + capabilities.getService().getName() + " doesn't "
          + "support HTTP POST for GetFeature requests";
      Debug.debugSimpleMessage( s );
      throw new WebServiceException( s );
    }

    // get DescribeFeatureType address
    dcp = request.getDescribeFeatureType().getDCPType();
    get = ( (HTTP)dcp[0].getProtocol() ).getGetOnlineResources();
    addresses.put( DESCRIBEFEATURETYPE, get[0] );

    if( request.getGetFeatureWithLock() != null )
    {
      // get GetFeatureWithLock address
      dcp = request.getGetFeatureWithLock().getDCPType();
      po = false;
      for( int i = 0; i < dcp.length; i++ )
      {
        get = ( (HTTP)dcp[i].getProtocol() ).getPostOnlineResources();
        if( get != null && get.length > 0 )
        {
          addresses.put( GETFEATUREWITHLOCK, get[0] );
          po = true;
        }
      }
      if( !po )
      {
        String s = "WFS: " + capabilities.getService().getName() + " doesn't "
            + "support HTTP POST for GetFeatureWithLock requests";
        Debug.debugSimpleMessage( s );
        throw new WebServiceException( s );
      }
    }

    if( request.getTransaction() != null )
    {
      // get Transaction address
      dcp = request.getTransaction().getDCPType();
      po = false;
      for( int i = 0; i < dcp.length; i++ )
      {
        get = ( (HTTP)dcp[i].getProtocol() ).getPostOnlineResources();
        if( get != null && get.length > 0 )
        {
          addresses.put( TRANSACTION, get[0] );
          po = true;
        }
      }
      if( !po )
      {
        String s = "WFS: " + capabilities.getService().getName() + " doesn't "
            + "support HTTP POST for Transaction requests";
        Debug.debugSimpleMessage( s );
        throw new WebServiceException( s );
      }
    }

    if( request.getLockFeature() != null )
    {
      // get LockFeature address
      dcp = request.getLockFeature().getDCPType();
      get = ( (HTTP)dcp[0].getProtocol() ).getGetOnlineResources();
      addresses.put( LOCKFEATURE, get[0] );
    }

  }

  /**
   * 
   * 
   * @param event
   *          request to be performed
   */
  public void doService( OGCWebServiceEvent event ) throws WebServiceException
  {
    OGCWebServiceRequest request = event.getRequest();
    OGCWebServiceClient client = event.getDestination();

    if( request instanceof WFSGetFeatureRequest )
    {
      handleGetFeature( (WFSGetFeatureRequest)request, client );
    }
    else if( request instanceof WFSDescribeFeatureTypeRequest )
    {
      handleDescribeFeatureType( (WFSDescribeFeatureTypeRequest)request, client );
    }
    else if( request instanceof WFSGetCapabilitiesRequest )
    {
      handleGetCapabilities( (WFSGetCapabilitiesRequest)request, client );
    }
    else if( request instanceof WFSLockFeatureRequest )
    {
      handleLockFeature( (WFSLockFeatureRequest)request );
    }
    else if( request instanceof WFSTransactionRequest )
    {
      handleTransaction( (WFSTransactionRequest)request );
    }

  }

  /**
   * the method performs the handling of the passed OGCWebServiceEvent directly
   * and returns the result to the calling class/method
   * 
   * @param request
   *          request (WMS, WCS, WFS, WCAS, WCTS, WTS, Gazetter) to perform
   * 
   * @throws WebServiceException
   */
  public OGCWebServiceResponse doService( OGCWebServiceRequest request ) throws WebServiceException
  {
    Debug.debugMethodBegin();
    Debug.debugMethodEnd();
    throw new NoSuchMethodError( "doService(OGCWebServiceRequest)" );
  }

  /**
   * performs a GetFeature request against the remote service. The method uses
   * http-POST to call the remote WFS
   * 
   * @param request
   *          get feature request
   */
  private void handleGetFeature( WFSGetFeatureRequest request, OGCWebServiceClient client )
      throws WebServiceException
  {
    Debug.debugMethodBegin( this, "handleGetFeature" );

    URL url = (URL)addresses.get( GETFEATURE );
    String param = ( (Marshallable)request ).exportAsXML();

    // create new Thread and start it
    new RemoteWFSHandler( request, client, url, param )
    {

      public void run()
      {

        OGCWebServiceException exce = null;
        GMLFeatureCollection result = null;
        try
        {
          // get map from the remote service
          NetWorker nw = new NetWorker( "UTF-8", laddress, lparam );
          String contentType = nw.getContentType();
          if( contentType == null || MimeTypeMapper.isKnownMimeType( contentType ) )
          {
            Document doc = null;
            try
            {
              InputStreamReader isr = new InputStreamReader( nw.getInputStream(), "UTF-8" );
              doc = XMLTools.parse( isr );
              result = new GMLFeatureCollection_Impl( doc.getDocumentElement() );
            }
            catch( Exception e )
            {
              throw new WebServiceException( e.toString() );
            }
          }
          else
          {
            exce = new OGCWebServiceException_Impl( "RemoteWFS:handleGetFeature",
                "Response of the remote " + "WFS contains unknown content " + "type: "
                    + contentType + ";request: " + lparam );
          }
        }
        catch( Exception e )
        {
          exce = new OGCWebServiceException_Impl( "RemoteWFS:handleGetFeature",
              "Could not get feature from RemoteWFS: " + capabilities.getService().getName()
                  + "; request: " + lparam + "; " + e.toString() );

        }

        OGCWebServiceResponse resp = WFSProtocolFactory.createWFSGetFeatureResponse( lrequest,
            ( (WFSGetFeatureRequest)lrequest ).getTypeNames(), exce, result );
        OGCWebServiceEvent event = new OGCWebServiceEvent_Impl( this, resp, "" );
        lclient.write( event );
      }
    }.start();

    Debug.debugMethodEnd();

  }

  /**
   * Pefroms a describe feature type request against a remote WFS. The method
   * uses http-GET to call the remote WFS
   * 
   * @param request
   *          describe feature type request
   * @param client
   *          receiver of the response to the request
   */
  private void handleDescribeFeatureType( WFSDescribeFeatureTypeRequest request,
      OGCWebServiceClient client ) throws WebServiceException
  {
    Debug.debugMethodBegin( this, "handleDescribeFeatureType" );

    URL url = (URL)addresses.get( DESCRIBEFEATURETYPE );

    String param = request.getRequestParameter();

    // create new Thread and start it
    new RemoteWFSHandler( request, client, url, param )
    {

      public void run()
      {

        OGCWebServiceException exce = null;
        String result = null;
        try
        {
          String remoteAddress = NetWorker.url2String( laddress );
          String us = remoteAddress + "?" + lparam;
          URL ur = new URL( us );
          // get map from the remote service
          NetWorker nw = new NetWorker( "UTF-8", ur );
          byte[] b = nw.getDataAsByteArr( 20000 );
          String contentType = nw.getContentType();
          if( MimeTypeMapper.isKnownMimeType( contentType ) )
          {
            // create a WFSCapabilities instance from the result
            result = new String( b );
          }
          else
          {
            exce = new OGCWebServiceException_Impl( "RemoteWFS:handleDescribeFeatureType",
                "Response of the remote " + "WFS contains unknown content " + "type: "
                    + contentType + ";request: " + lparam );
          }
        }
        catch( Exception e )
        {
          exce = new OGCWebServiceException_Impl( "RemoteWFS:handleDescribeFeatureType",
              "Could not get map from RemoteWFS: " + capabilities.getService().getName()
                  + "; request: " + lparam + "; " + e.toString() );

        }

        OGCWebServiceResponse resp = WFSProtocolFactory.createWFSGetFeatureResponse( lrequest,
            null, exce, result );
        OGCWebServiceEvent event = new OGCWebServiceEvent_Impl( this, resp, "" );

        lclient.write( event );
      }
    }.start();

    Debug.debugMethodEnd();
  }

  /**
   * reads the capabilities from the remote WFS by performing a GetCapabilities
   * request against it. The method uses http-GET to call the remote WFS
   * 
   * @param request
   *          capabilities request
   * @param client
   *          receiver of the response to the request
   */
  private void handleGetCapabilities( WFSGetCapabilitiesRequest request, OGCWebServiceClient client )
      throws WebServiceException
  {
    Debug.debugMethodBegin();

    URL url = (URL)addresses.get( GETCAPABILITIES );
    String param = request.getRequestParameter();

    // create new Thread and start it
    new RemoteWFSHandler( request, client, url, param )
    {

      public void run()
      {

        OGCWebServiceException exce = null;
        WFSCapabilities result = null;
        try
        {
          String remoteAddress = NetWorker.url2String( laddress );
          String us = remoteAddress + "?" + lparam;
          URL ur = new URL( us );
          // get map from the remote service
          NetWorker nw = new NetWorker( "UTF-8", ur );
          byte[] b = nw.getDataAsByteArr( 20000 );
          String contentType = nw.getContentType();
          if( MimeTypeMapper.isKnownMimeType( contentType ) )
          {
            // create a WFSCapabilities instance from the result
            StringReader reader = new StringReader( new String( b ) );
            result = WFSCapabilitiesFactory.createCapabilities( reader );
          }
          else
          {
            exce = new OGCWebServiceException_Impl( "RemoteWFS:handleGetCapabilities",
                "Response of the remote " + "WFS contains unknown content " + "type: "
                    + contentType + ";request: " + lparam );
          }
        }
        catch( Exception e )
        {
          exce = new OGCWebServiceException_Impl( "RemoteWFS:handleGetCapabilities",
              "Could not get map from RemoteWFS: " + capabilities.getService().getName()
                  + "; request: " + lparam + "; " + e.toString() );

        }

        OGCWebServiceResponse resp = WFSProtocolFactory.createWFSGetFeatureResponse( lrequest,
            null, exce, result );
        OGCWebServiceEvent event = new OGCWebServiceEvent_Impl( this, resp, "" );

        lclient.write( event );
      }
    }.start();

    Debug.debugMethodEnd();

  }

  /**
   * 
   * 
   * @param request
   */
  private OGCWebServiceEvent handleLockFeature( WFSLockFeatureRequest request )
  {
    // FIXME
    // TODO
    return null;
  }

  /**
   * 
   * 
   * @param request
   */
  private OGCWebServiceEvent handleTransaction( WFSTransactionRequest request )
  {
    // FIXME
    // TODO
    return null;
  }

  ///////////////////////////////////////////////////////////////////////////
  //                    inner classes //
  ///////////////////////////////////////////////////////////////////////////
  protected abstract class RemoteWFSHandler extends Thread
  {

    protected OGCWebServiceClient lclient = null;

    protected URL laddress = null;

    protected String lparam = null;

    protected OGCWebServiceRequest lrequest = null;

    RemoteWFSHandler( OGCWebServiceRequest request, OGCWebServiceClient client, URL address,
        String param )
    {
      this.lclient = client;
      this.laddress = address;
      this.lparam = param;
      this.lrequest = request;
    }

    protected String getInputStreamContent( InputStream is ) throws IOException
    {
      StringBuffer sb = new StringBuffer( 1000 );
      int c = 0;
      while( ( c = is.read() ) >= 0 )
      {
        sb.append( (char)c );
      }
      is.close();
      return sb.toString();
    }

  }

}