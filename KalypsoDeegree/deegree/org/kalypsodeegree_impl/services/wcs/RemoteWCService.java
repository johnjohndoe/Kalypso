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
package org.deegree_impl.services.wcs;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import org.deegree.services.OGCWebServiceEvent;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.OGCWebServiceResponse;
import org.deegree.services.WebServiceException;
import org.deegree.services.wcs.protocol.WCSDescribeCoverageLayerRequest;
import org.deegree.services.wcs.protocol.WCSGetCapabilitiesRequest;
import org.deegree.services.wcs.protocol.WCSGetCoverageRequest;
import org.deegree.services.wcs.protocol.WCSGetCoverageResponse;
import org.deegree_impl.services.OGCWebService_Impl;
import org.deegree_impl.services.wcs.protocol.WCSProtocolFactory;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.NetWorker;

/**
 * An instance of the class acts as a wrapper to a remote WCS.
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class RemoteWCService extends OGCWebService_Impl
{
  private String remoteAddress = null;

  /** Creates a new instance of RemoteWCService */
  public RemoteWCService( String remoteAddress )
  {
    this.remoteAddress = remoteAddress;
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

    WCSGetCoverageResponse response = null;
    if( request instanceof WCSGetCoverageRequest )
    {
      Object result = handleGetCoverage( (WCSGetCoverageRequest)request );
      response = WCSProtocolFactory.createGetCoverageResponse( event.getRequest(), (byte[])result );
    }
    else if( request instanceof WCSDescribeCoverageLayerRequest )
    {
      //Object result =
      // handleDescribeCoverageLayer((WCSDescribeCoverageLayerRequest) request);
    }
    else if( request instanceof WCSGetCapabilitiesRequest )
    {
      //Object result = handleGetCapabilities((WCSGetCapabilitiesRequest)
      // request);
    }

    event.getDestination().write( response );
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
   * performs a GetMap request against the remote service. The result contains
   * the map decoded in the desired format as a byte array.
   * 
   * @param request
   *          GetMap request
   * @return map (image) in the desired format as byte array
   */
  private OGCWebServiceEvent handleGetCoverage( WCSGetCoverageRequest request )
      throws WebServiceException
  {
    Debug.debugMethodBegin( this, "handleGetCoverage" );

    String param = request.getRequestParameter();
    String us = remoteAddress + "?" + param;
    URL url = null;
    try
    {
      url = new URL( us );
    }
    catch( Exception e )
    {
      throw new WebServiceException( e.getMessage() );
    }

    // get map from the remote service
    byte[] result = null;
    try
    {
      NetWorker nw = new NetWorker( url );
      result = nw.getDataAsByteArr( 20000 );
    }
    catch( Exception e )
    {
      System.out.println( e );
    }
    //TODO create result

    Debug.debugMethodEnd();
    return null;
  }

  /**
   * reads feature infos from the remote WCS by performing a FeatureInfo request
   * against it. As long the result of a FeatureInfo request is generic (for
   * usual it is som HTML) it isn't easy to combine the result with that of
   * other WCS's
   * 
   * @param request
   *          feature info request
   * @return feaure info(s) decoded in the requested fromat
   */
  private OGCWebServiceEvent handleDescribeCoverageLayer( WCSDescribeCoverageLayerRequest request )
      throws WebServiceException
  {
    Debug.debugMethodBegin( this, "handleDescribeCoverageLayer" );

    String param = request.getRequestParameter();
    String us = remoteAddress + "?" + param;
    URL url = null;
    try
    {
      url = new URL( us );
    }
    catch( Exception e )
    {
      throw new WebServiceException( e.getMessage() );
    }

    // read feature info from stream
    InputStream is = null;
    StringBuffer result = new StringBuffer( 10000 );
    try
    {
      NetWorker nw = new NetWorker( url );
      is = nw.getInputStream();
      int c = 0;
      while( ( c = is.read() ) >= 0 )
      {
        result.append( (char)c );
      }
      is.close();
    }
    catch( Exception e )
    {
      try
      {
        if( is != null )
          is.close();
      }
      catch( IOException ei )
      {
        System.out.println( ei );
      }
      throw new WebServiceException( e.getMessage() );
    }

    Debug.debugMethodEnd();
    return null;
  }

  /**
   * reads the capabilities from the remote WCS by performing a GetCapabilities
   * request against it.
   * 
   * @param request
   *          capabilities request
   */
  private OGCWebServiceEvent handleGetCapabilities( WCSGetCapabilitiesRequest request )
      throws WebServiceException
  {
    Debug.debugMethodBegin( this, "handleGetCapabilities" );

    String param = request.getRequestParameter();
    String us = remoteAddress + "?" + param;
    URL url = null;
    try
    {
      url = new URL( us );
    }
    catch( Exception e )
    {
      throw new WebServiceException( e.getMessage() );
    }

    // read capabilities from stream
    InputStream is = null;
    StringBuffer result = new StringBuffer( 10000 );
    try
    {
      NetWorker nw = new NetWorker( url );
      is = nw.getInputStream();
      int c = 0;
      while( ( c = is.read() ) >= 0 )
      {
        result.append( (char)c );
      }
      is.close();
    }
    catch( Exception e )
    {
      try
      {
        if( is != null )
          is.close();
      }
      catch( IOException ei )
      {
        System.out.println( ei );
      }
      throw new WebServiceException( e.getMessage() );
    }

    Debug.debugMethodEnd();
    return null;
  }

}