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

import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.net.URL;
import java.util.HashMap;

import org.deegree.graphics.Encoders;
import org.deegree.model.coverage.CVDescriptor;
import org.deegree.model.coverage.CoverageLayer;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.services.Handler;
import org.deegree.services.OGCWebServiceClient;
import org.deegree.services.OGCWebServiceEvent;
import org.deegree.services.OGCWebServiceException;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.OGCWebServiceResponse;
import org.deegree.services.RangeParamList;
import org.deegree.services.WebServiceException;
import org.deegree.services.wcs.capabilities.WCSCapabilities;
import org.deegree.services.wcs.protocol.WCSDescribeCoverageLayerRequest;
import org.deegree.services.wcs.protocol.WCSGetCapabilitiesRequest;
import org.deegree.services.wcs.protocol.WCSGetCoverageRequest;
import org.deegree.services.wcs.protocol.WCSGetCoverageResponse;
import org.deegree.xml.XMLParsingException;
import org.deegree_impl.io.geotiff.GeoTiffWriter;
import org.deegree_impl.model.cs.Adapters;
import org.deegree_impl.model.cs.ConvenienceCSFactory;
import org.deegree_impl.model.cs.CoordinateSystem;
import org.deegree_impl.model.cv.CVDescriptorDirFactory;
import org.deegree_impl.model.cv.CVDescriptorFactory;
import org.deegree_impl.model.gc.GC_GridCoverage_Impl;
import org.deegree_impl.services.NotSupportedFormatException;
import org.deegree_impl.services.OGCWebServiceEvent_Impl;
import org.deegree_impl.services.OGCWebServiceException_Impl;
import org.deegree_impl.services.OGCWebService_Impl;
import org.deegree_impl.services.wcs.protocol.WCSProtocolFactory;
import org.deegree_impl.tools.Debug;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * 
 * This is the basic implementation for a OGC conform web coverage service
 * within the deegree framework. A <tt>WCSService</tt> extends the
 * <tt>OGCWebService</tt> interface to act like a OGC web service. This means
 * that a WCS is callable through the <tt>doService</tt> -method inherited
 * from <tt>OGCWebService</tt>.
 * 
 * <p>
 * -----------------------------------------------------
 * </p>
 * 
 * @author Andreas Poth
 * @version $Revision$ $Date$
 *          <p>
 */
public class WCService_Impl extends OGCWebService_Impl implements Handler
{
  private HashMap descriptors = null;

  /**
   * Creates a new WCService_Impl object.
   * 
   * @param capabilities
   *          capabilites of the WCS
   * 
   * @throws XMLParsingException
   */
  public WCService_Impl( WCSCapabilities capabilities ) throws XMLParsingException
  {
    Debug.debugMethodBegin( this, "WCService_Impl" );

    try
    {
      this.descriptors = new HashMap( 50 );

      CoverageLayer[] cl = capabilities.getCoverageLayerList();
      for( int i = 0; i < cl.length; i++ )
      {
        URL url = cl[i].getDescriptorResource();
        String name = cl[i].getLayerID();
        CVDescriptor desc = null;

        // 'DD' indicates that coordinates are used to name the tiles
        // the layer is splitted in
        if( name.toUpperCase().startsWith( "DD" ) )
        {
          desc = CVDescriptorDirFactory.createCVDescriptor( url );
        }
        else
        {
          // quadtree based index
          desc = CVDescriptorFactory.createCVDescriptor( url );
        }
        descriptors.put( name, desc );
      }
    }
    catch( Exception e )
    {
      throw new XMLParsingException( "Could not create grid descriptors: " + e.toString(), e );
    }

    Debug.debugMethodEnd();
  }

  /**
   * Creates a new WCService_Impl object.
   * 
   * @param descriptors
   */
  public WCService_Impl( CVDescriptor[] descriptors )
  {
    this.descriptors = new HashMap( descriptors.length );

    for( int i = 0; i < descriptors.length; i++ )
    {
      this.descriptors.put( descriptors[i].getCoverageLayer().getLayerID(), descriptors[i] );
    }
  }

  /**
   * returns a copy of the WCService_Impl using the same capabilities reference
   * 
   * @return
   */
  public Object clone()
  {
    CVDescriptor[] desc = new CVDescriptor[descriptors.size()];
    desc = (CVDescriptor[])descriptors.values().toArray( desc );
    return new WCService_Impl( desc );
  }

  /**
   * 
   * 
   * @param event
   * 
   * @throws WebServiceException
   */
  public void doService( OGCWebServiceEvent event ) throws WebServiceException
  {
    Debug.debugMethodBegin();
    handleRequest( event );
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
   * handles a request against an OGC web service
   *  
   */
  public void handleRequest( OGCWebServiceEvent event )
  {
    Debug.debugMethodBegin( this, "handleRequest" );

    OGCWebServiceRequest request = event.getRequest();
    WCSGetCoverageResponse response = null;

    if( request instanceof WCSGetCoverageRequest )
    {
      response = handleGetCoverageRequest( event );
      if( response == null ) //ETJ EXT
        System.out.println( "ETJ: Null response from getCoverage" );
    }
    else if( request instanceof WCSGetCapabilitiesRequest )
    {
      // TODO GetCapabilities
      Exception ex = new Exception( "not supported operation" );
      response = createErrorResponse( request, ex, "handleRequest" );
    }
    else if( request instanceof WCSDescribeCoverageLayerRequest )
    {
      // TODO DescribeCoverage
      Exception ex = new Exception( "not supported operation" );
      response = createErrorResponse( request, ex, "handleRequest" );
    }

    // write response to the requesting client
    OGCWebServiceClient client = event.getDestination();
    OGCWebServiceEvent event_ = new OGCWebServiceEvent_Impl( this, response, "" );
    client.write( event_ );

    Debug.debugMethodEnd();
  }

  /**
   * handles a GetCoverage request
   */
  private WCSGetCoverageResponse handleGetCoverageRequest( OGCWebServiceEvent event )
  {
    Debug.debugMethodBegin( this, "handleGetCoverageRequest" );

    WCSGetCoverageRequest gcr = (WCSGetCoverageRequest)event.getRequest();
    CVDescriptor desc = (CVDescriptor)descriptors.get( gcr.getLayer() );
    int width = gcr.getWidth();
    int height = gcr.getHeight();
    GM_Envelope env = gcr.getBoundingBox();
    String format = gcr.getFormat().toUpperCase();
    int type = 0;
    RangeParamList ranges = gcr.getRangeList();
    // ETJ FIXME this gcr may be originated by WMS: maybe not all WMS ranges are
    // meaningful for WCS.

    //		RangeSetDescription rsd =
    // ((GridCoverageLayer)desc.getCoverageLayer()).getRangeSetDescription();
    //		GridRangeDescription[] grd = rsd.getGridRangeDescription();
    //		if(grd.length == 0) System.out.println(" === No Grid Range defined");
    //		else
    //			for (int i = 0; i < grd.length; i++)
    //			{
    //				System.out.println("GRD["+i+"] = "+grd[i].getID());
    //			}
    //
    //
    //		String[] range = gcr.getRange();
    //		if(range.length == 0)
    //			System.out.println("*** No req ranges defined");
    //
    //		for (int i = 0; i < range.length; i++)
    //		{
    //			System.out.println(" === RANGE: "+range[i]);
    //			String[][] kkk = gcr.getParam(range[i]);
    //
    //			for (int j = 0; j < kkk.length; j++)
    //			{
    //				for (int k = 0; k < kkk[j].length; k++)
    //				{
    //					System.out.println("PARAM["+j+"]["+k+"]="+kkk[j][k]);
    //				}
    //			}
    //		}

    if( format.equals( "BMP" ) )
    {
      type = BufferedImage.TYPE_INT_RGB;
    }
    else if( format.equals( "PNG" ) )
    {
      type = BufferedImage.TYPE_INT_ARGB;
    }
    else if( format.equals( "GIF" ) )
    {
      type = BufferedImage.TYPE_INT_ARGB;
    }
    else if( format.equals( "TIF" ) || format.equalsIgnoreCase( "TIFF" ) )
    {
      type = BufferedImage.TYPE_3BYTE_BGR;
    }
    else if( format.equals( "JPG" ) || format.equalsIgnoreCase( "JPEG" ) )
    {
      type = BufferedImage.TYPE_INT_RGB;
    }
    else if( format.equals( "JAVAIMAGE" ) )
    {
      type = BufferedImage.TYPE_INT_ARGB;
    }

    WCSGetCoverageResponse response = null;

    // get image from a GC_GridCoverage object
    Object image = null;
    try
    {
      GC_GridCoverage_Impl gc = new GC_GridCoverage_Impl( desc, false );
      image = gc.getRaster( env, width, height, type, ranges );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      response = createErrorResponse( event.getRequest(), e, "handleGetCoverageRequest" );
    }

    // serialize image to a byte array and create the response object
    try
    {
      if( image != null )
      {
        String srs = gcr.getSRS();
        CoordinateSystem cs = ConvenienceCSFactory.getInstance().getCSByName( srs );
        CS_CoordinateSystem crs = Adapters.getDefault().export( cs );
        Object out = getSerializedImage( image, gcr.getFormat().toUpperCase(), env, crs );
        response = WCSProtocolFactory.createGetCoverageResponse( event.getRequest(), out );
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
      response = createErrorResponse( event.getRequest(), e, "handleGetCoverageRequest" );
    }

    Debug.debugMethodEnd();
    return response;
  }

  /**
   * returns a response object that contains an error message
   */
  private WCSGetCoverageResponse createErrorResponse( OGCWebServiceRequest request, Exception ex,
      String location )
  {
    Debug.debugMethodBegin( this, "createErrorResponse" );

    OGCWebServiceException excep = new OGCWebServiceException_Impl( location, ex.getMessage() );

    WCSGetCoverageResponse resp = WCSProtocolFactory.createGetCoverageResponse( request, excep );

    Debug.debugMethodEnd();
    return resp;
  }

  /**
   * serializes the submitted image to a byte array. the format of the
   * serialization depends on the also submitted format. at the moment gif,
   * jpeg, tiff, png and bmp are supported.
   */
  private Object getSerializedImage( Object image, String format, GM_Envelope env,
      CS_CoordinateSystem crs ) throws NotSupportedFormatException, Exception
  {
    Debug.debugMethodBegin();

    ByteArrayOutputStream bos = null;
    Object out = image;

    if( format.equals( "BMP" ) )
    {
      BufferedImage bi = (BufferedImage)image;

      bos = new ByteArrayOutputStream( bi.getWidth() * bi.getHeight() * 3 );
      Encoders.encodeBmp( bos, bi );
      out = bos.toByteArray();
      bos.close();
    }
    else if( format.equals( "PNG" ) )
    {
      BufferedImage bi = (BufferedImage)image;

      bos = new ByteArrayOutputStream( bi.getWidth() * bi.getHeight() * 4 );
      Encoders.encodePng( bos, bi );
      out = bos.toByteArray();

      bos.close();
    }
    else if( format.equals( "GIF" ) )
    {
      BufferedImage bi = (BufferedImage)image;

      bos = new ByteArrayOutputStream( bi.getWidth() * bi.getHeight() );
      Encoders.encodeGif( bos, bi );
      out = bos.toByteArray();
      bos.close();
    }
    else if( format.equalsIgnoreCase( "TIF" ) || format.equalsIgnoreCase( "TIFF" ) )
    {
      BufferedImage bi = (BufferedImage)image;

      bos = new ByteArrayOutputStream( bi.getWidth() * bi.getHeight() * 4 );
      GeoTiffWriter gw = new GeoTiffWriter( bi, env, env.getWidth() / bi.getWidth(), env
          .getHeight()
          / bi.getHeight(), crs );
      gw.write( bos );
      //Encoders.encodeTiff( bos, bi );
      out = bos.toByteArray();
      bos.close();
    }
    else if( format.equals( "JPG" ) || format.equals( "JPEG" ) )
    {
      BufferedImage bi = (BufferedImage)image;

      bos = new ByteArrayOutputStream( bi.getWidth() * bi.getHeight() * 3 );
      Encoders.encodeJpeg( bos, bi, 0.95f );
      out = bos.toByteArray();
      bos.close();
    }
    else if( format.toUpperCase().startsWith( "JAVAIMAGE" ) )
    {
      // "JAVAIMAGE" indicates that the request is comming from a local client
      // so no image serialization to a graphic fromat is required
      out = image;
    }
    else if( format.equalsIgnoreCase( "IMG" ) )
    {
      float[][] data = (float[][])image;

      if( data.length > 0 )
      {
        bos = new ByteArrayOutputStream( ( data.length * data[0].length * 4 ) + 1 );
      }
      else
      {
        bos = new ByteArrayOutputStream( 40 + 1 );
      }

      DataOutputStream os = new DataOutputStream( bos );
      for( int i = 0; i < data.length; i++ )
      {
        for( int j = 0; j < data[i].length; j++ )
        {
          os.writeFloat( data[i][j] );
        }
      }
      out = bos.toByteArray();
      os.close();
      bos.close();
    }

    Debug.debugMethodEnd();
    return out;
  }

  /**
   * handles the response of an OGC web service
   *  
   */
  public void handleResponse( OGCWebServiceEvent response )
  {
    throw new NoSuchMethodError( "method: 'handleResponse' is not implemented "
        + "at WCService_Impl" );
  }

  /**
   * returns true if the handler is interested in a event
   *  
   */
  public boolean isInterested( OGCWebServiceEvent event )
  {
    Debug.debugMethodBegin( this, "handleRequest" );

    boolean interested = false;
    OGCWebServiceRequest req = event.getRequest();

    if( ( req != null ) && req instanceof WCSGetCoverageRequest )
    {
      WCSGetCoverageRequest gcr = (WCSGetCoverageRequest)req;
      interested = descriptors.get( gcr.getLayer() ) != null;
    }

    Debug.debugMethodEnd();
    return interested;
  }

  /**
   * registers a Handler so this Handler is able to act as a proxy to the
   * registered handler
   *  
   */
  public void registerHandler( Handler handler )
  {
    throw new NoSuchMethodError( "method: 'registerHandler' is not implemented "
        + "at WCService_Impl" );
  }

  /**
   * @see #registerHandler(Handler)
   *  
   */
  public void removeHandler( Handler handler )
  {
    throw new NoSuchMethodError( "method: 'removeHandler' is not implemented "
        + "at WCService_Impl" );
  }
}