// $Header:
// /cvsroot/deegree/deegree/org/deegree_impl/services/wms/WMService_Impl.java,v
// 1.33 2004/05/26 08:50:48 poth Exp $
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

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import org.deegree.services.OGCWebServiceEvent;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.OGCWebServiceResponse;
import org.deegree.services.WebServiceException;
import org.deegree.services.wms.CurrentUpdateSequenceException;
import org.deegree.services.wms.GetFeatureInfoHandler;
import org.deegree.services.wms.GetMapHandler;
import org.deegree.services.wms.InvalidUpdateSequenceException;
import org.deegree.services.wms.capabilities.Operation;
import org.deegree.services.wms.capabilities.Request;
import org.deegree.services.wms.capabilities.WMSCapabilities;
import org.deegree.services.wms.protocol.WMSDescribeLayerRequest;
import org.deegree.services.wms.protocol.WMSGetCapabilitiesRequest;
import org.deegree.services.wms.protocol.WMSGetCapabilitiesResponse;
import org.deegree.services.wms.protocol.WMSGetFeatureInfoRequest;
import org.deegree.services.wms.protocol.WMSGetFeatureInfoResponse;
import org.deegree.services.wms.protocol.WMSGetLegendGraphicRequest;
import org.deegree.services.wms.protocol.WMSGetLegendGraphicResponse;
import org.deegree.services.wms.protocol.WMSGetMapRequest;
import org.deegree.services.wms.protocol.WMSGetMapResponse;
import org.deegree.services.wms.protocol.WMSGetScaleBarRequest;
import org.deegree.services.wms.protocol.WMSGetStylesRequest;
import org.deegree.services.wms.protocol.WMSGetStylesResponse;
import org.deegree.services.wms.protocol.WMSPutStylesRequest;
import org.deegree_impl.services.OGCWebServiceEvent_Impl;
import org.deegree_impl.services.OGCWebService_Impl;
import org.deegree_impl.services.wms.protocol.WMSProtocolFactory;
import org.deegree_impl.tools.Debug;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author last edited by: $Author$
 * 
 * @version 1.0. $Revision$, $Date$
 * 
 * @since 1.1
 */
public class WMService_Impl extends OGCWebService_Impl
{
  private static WMSCache cache = null;

  private WMSCapabilities capabilities = null;

  /**
   * Creates a new WMService_Impl object.
   * 
   * @param capabilities
   */
  public WMService_Impl( WMSCapabilities capabilities )
  {
    this.capabilities = capabilities;
    cache = WMSCache.getInstance( capabilities.getDeegreeParam().getCacheSize() * 1000 );
  }

  /**
   * receives a WMS request and deligates it to the responsible handle-methods
   * 
   * @param event
   *          any WMS-request
   * 
   * @throws WebServiceException
   */
  public void doService( OGCWebServiceEvent event ) throws WebServiceException
  {
    Debug.debugMethodBegin( this, "doService" );

    OGCWebServiceRequest request = event.getRequest();

    //WMSCache cache = WMSCache.getInstance(
    // capabilities.getDeegreeParam().getCacheSize() );
    OGCWebServiceResponse result = null;

    if( request instanceof WMSGetMapRequest )
    {
      // try retrieving result from the cache
      result = cache.get( (WMSGetMapRequest)request );

      // if result can't be retrieved from the cache perform query
      if( result == null )
      {
        GetMapHandler gmh = (GetMapHandler)createHandler( request, WMSGetMapRequest.class,
            Operation.GETMAP );
        result = gmh.performGetMap();

        if( ( (WMSGetMapResponse)result ).getMap() != null )
        {
          //cache.push( request, ((WMSGetMapResponse)result).getMap() );
        }
      }
    }
    else if( request instanceof WMSGetFeatureInfoRequest )
    {
      // try retrieving result from the cache
      result = cache.get( (WMSGetFeatureInfoRequest)request );

      // if result can't be retrieved from the cache perform query
      if( result == null )
      {
        GetFeatureInfoHandler gmh = (GetFeatureInfoHandler)createHandler( request,
            WMSGetFeatureInfoRequest.class, Operation.GETFEATUREINFO );
        result = gmh.performGetFeatureInfo();

        if( ( (WMSGetFeatureInfoResponse)result ).getFeatureInfo() != null )
        {
          cache.push( request, ( (WMSGetFeatureInfoResponse)result ).getFeatureInfo() );
        }
      }
    }
    else if( request instanceof WMSGetCapabilitiesRequest )
    {
      result = handleGetCapabilities( (WMSGetCapabilitiesRequest)request );
    }
    else if( request instanceof WMSGetStylesRequest )
    {
      handleGetStyles( (WMSGetStylesRequest)request );
    }
    else if( request instanceof WMSPutStylesRequest )
    {
      handlePutStyles( (WMSPutStylesRequest)request );
    }
    else if( request instanceof WMSDescribeLayerRequest )
    {
      handleDescribeLayer( (WMSDescribeLayerRequest)request );
    }
    else if( request instanceof WMSGetScaleBarRequest )
    {
      result = handleGetScaleBar( (WMSGetScaleBarRequest)request );
    }
    else if( request instanceof WMSGetLegendGraphicRequest )
    {
      result = handleGetLegendGraphic( (WMSGetLegendGraphicRequest)request );
    }

    OGCWebServiceEvent event_ = new OGCWebServiceEvent_Impl( this, result, "" );
    event.getDestination().write( event_ );

    Debug.debugMethodEnd();
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
   * creates a handler class for performing the incomming request. The instance
   * that will be created depends on the responsible class the for the submitted
   * request in the WMS capabilities/configuration.
   * 
   * @param request
   *          request to be performed
   * @param requestClass
   *          of the request (WMSGetStylesRequest, WMSFeatureInfoRequest etc.)
   * @param operationType
   *          type of the operation to perform by the handler
   */
  private Object createHandler( OGCWebServiceRequest request, Class requestClass, int operationType )
      throws WebServiceException
  {
    Debug.debugMethodBegin( this, "createGetMapHandler" );

    // describes the signature of the required constructor
    Class[] cl = new Class[2];
    cl[0] = WMSCapabilities.class;
    cl[1] = requestClass;

    // set parameter to submitt to the constructor
    Object[] o = new Object[2];
    o[0] = capabilities;
    o[1] = request;

    Object handler = null;
    Request req = capabilities.getCapability().getRequest();
    String className = req.getOperation( operationType ).getResponsibleClass();

    try
    {
      // get constructor
      Class creator = Class.forName( className );
      Constructor con = creator.getConstructor( cl );

      // call constructor and instantiate a new DataStore
      handler = con.newInstance( o );
    }
    catch( ClassNotFoundException cce )
    {
      throw new WebServiceException( "Couldn't instantiate " + className + "! \n" + cce.toString() );
    }
    catch( NoSuchMethodException nsme )
    {
      throw new WebServiceException( "Couldn't instantiate " + className + "! \n" + nsme.toString() );
    }
    catch( InstantiationException ie )
    {
      throw new WebServiceException( "Couldn't instantiate " + className + "! \n" + ie.toString() );
    }
    catch( IllegalAccessException iae )
    {
      throw new WebServiceException( "Couldn't instantiate " + className + "! \n" + iae.toString() );
    }
    catch( InvocationTargetException ite )
    {
      throw (WebServiceException)ite.getCause();
    }
    catch( Exception e )
    {
      throw new WebServiceException( "Couldn't instantiate " + className + "! \n" + e.toString() );
    }

    Debug.debugMethodEnd();

    return handler;
  }

  /**
   * reads/creates the capabilities of the WMS.
   * 
   * @param request
   *          capabilities request
   */
  private WMSGetCapabilitiesResponse handleGetCapabilities( WMSGetCapabilitiesRequest request )
      throws WebServiceException
  {
    Debug.debugMethodBegin( this, "handleGetCapabilities" );

    String rUp = request.getUpdateSequence();
    String cUp = capabilities.getUpdateSequence();

    if( ( rUp != null ) && ( cUp != null ) && ( rUp.compareTo( cUp ) == 0 ) )
    {
      throw new CurrentUpdateSequenceException( "request update sequence: " + rUp
          + "is equal to capabilities" + " update sequence " + cUp );
    }

    if( ( rUp != null ) && ( cUp != null ) && ( rUp.compareTo( cUp ) > 0 ) )
    {
      throw new InvalidUpdateSequenceException( "request update sequence: " + rUp
          + " is higher then the " + "capabilities update sequence " + cUp );
    }

    WMSGetCapabilitiesResponse res = null;
    res = WMSProtocolFactory.createGetCapabilitiesResponse( request, null, capabilities );

    Debug.debugMethodEnd();
    return res;
  }

  /**
   * 
   * 
   * @param request
   *          get styles request (WMS 1.1.1 - SLD)
   */
  private WMSGetStylesResponse handleGetStyles( WMSGetStylesRequest request )
  {
    // FIXME
    // TODO
    return null;
  }

  /**
   * 
   * 
   * @param request
   *          put styles request (WMS 1.1.1 - SLD)
   */
  private void handlePutStyles( WMSPutStylesRequest request )
  {
  // FIXME
  // TODO
  }

  /**
   * 
   * 
   * @param request
   *          describe layer request (WMS 1.1.1 - SLD)
   */
  private void handleDescribeLayer( WMSDescribeLayerRequest request )
  {
  // FIXME
  // TODO
  }

  /**
   * 
   * 
   * @param request
   * 
   * @return @throws
   *         WebServiceException
   */
  private OGCWebServiceResponse handleGetLegendGraphic( WMSGetLegendGraphicRequest request )
      throws WebServiceException
  {

    Debug.debugMethodBegin();
    // try retrieving result from the cache
    OGCWebServiceResponse result = cache.get( request );

    if( result == null )
    {
      GetLegendGraphicHandler glgh = (GetLegendGraphicHandler)createHandler( request,
          WMSGetLegendGraphicRequest.class, Operation.GETLEGENDGRAPHIC );
      result = glgh.performGetLegendGraphic();
      if( ( (WMSGetLegendGraphicResponse)result ).getLegendGraphic() != null )
      {
        cache.push( request, ( (WMSGetLegendGraphicResponse)result ).getLegendGraphic() );
      }
    }

    Debug.debugMethodEnd();
    return result;
  }

  private OGCWebServiceResponse handleGetScaleBar( WMSGetScaleBarRequest request )
      throws WebServiceException
  {
    Debug.debugMethodBegin();

    OGCWebServiceResponse result = null;

    GetScaleBarHandler gsbh = (GetScaleBarHandler)createHandler( request,
        WMSGetScaleBarRequest.class, Operation.GETSCALEBAR );
    result = gsbh.performGetScaleBar();

    Debug.debugMethodEnd();
    return result;
  }
}
/*******************************************************************************
 * Changes to this class. What the people have been up to: $Log:
 * WMService_Impl.java,v $ Revision 1.33 2004/05/26 08:50:48 poth no message
 * 
 * Revision 1.32 2004/04/27 15:40:38 poth no message
 * 
 * Revision 1.31 2004/04/02 06:41:56 poth no message
 * 
 * Revision 1.30 2004/03/31 07:12:07 poth no message
 * 
 * 
 *  
 ******************************************************************************/