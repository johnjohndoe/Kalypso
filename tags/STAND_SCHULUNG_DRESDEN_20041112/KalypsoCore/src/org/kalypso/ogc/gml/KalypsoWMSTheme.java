package org.kalypso.ogc.gml;

import java.awt.Graphics;
import java.awt.Image;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Date;
import java.util.HashMap;
import java.util.Properties;

import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.services.OGCWebServiceClient;
import org.deegree.services.OGCWebServiceEvent;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.OGCWebServiceResponse;
import org.deegree.services.WebServiceException;
import org.deegree.services.wms.capabilities.WMSCapabilities;
import org.deegree.services.wms.protocol.WMSGetMapResponse;
import org.deegree.xml.XMLParsingException;
import org.deegree_impl.gml.schema.XMLHelper;
import org.deegree_impl.services.OGCWebServiceEvent_Impl;
import org.deegree_impl.services.wms.RemoteWMService;
import org.deegree_impl.services.wms.capabilities.OGCWMSCapabilitiesFactory;
import org.deegree_impl.services.wms.protocol.WMSProtocolFactory;
import org.deegree_impl.tools.NetWorker;
import org.kalypso.java.util.PropertiesHelper;
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Document;

/**
 * @author doemming
 */
public class KalypsoWMSTheme extends AbstractKalypsoTheme implements OGCWebServiceClient
{
  private final String m_layers;

  private Image myImage = null;

  private GM_Envelope myEnv = null;

  private String my_requestId = null;

  private GM_Envelope my_requestBBox = null;

  private final CS_CoordinateSystem m_crs;

  private RemoteWMService m_remoteWMS;

  public KalypsoWMSTheme( final String themeName, final String source, final CS_CoordinateSystem crs )
  {
    super( themeName );

    final Properties sourceProps = PropertiesHelper.parseFromString( source, '#' );
    
    m_crs = crs;
    m_layers = sourceProps.getProperty( "LAYERS", "" );
    
    final String service = sourceProps.getProperty( "SERVICE", "" );

    // TODO: maybe do this in a thread
    try
    {
      final OGCWMSCapabilitiesFactory wmsCapFac = new OGCWMSCapabilitiesFactory();

      final URL url = new URL( service + "SERVICE=WMS&VERSION=1.1.1&REQUEST=GetCapabilities" );

      final URLConnection c = url.openConnection();
      NetWorker.configureProxy( c );
      
      c.addRequestProperty( "SERVICE", "WMS" );
      c.addRequestProperty( "VERSION", "1.1.1" );
      c.addRequestProperty( "REQUEST", "GetCapabilities" );
      final Reader reader = new InputStreamReader( c.getInputStream() );

      final WMSCapabilities wmsCaps = wmsCapFac.createCapabilities( reader );
      m_remoteWMS = new RemoteWMService( wmsCaps );
    }
    catch( MalformedURLException e )
    {
      e.printStackTrace();
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
    catch( XMLParsingException e )
    {
      e.printStackTrace();
    }
    catch( WebServiceException e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paintSelected(java.awt.Graphics,
   *      org.deegree.graphics.transformation.GeoTransform, double,
   *      org.deegree.model.geometry.GM_Envelope, int)
   */
  public void paintSelected( Graphics g, GeoTransform p, double scale, GM_Envelope bbox,
      int selectionId )
  {
    // kann keine selektion zeichnen!
    if( selectionId != -1 )
      return;

    if( myEnv != null && myEnv.equals( bbox ) && myImage != null )
    {
      g.setPaintMode();
      g.drawImage( myImage, 0, 0, null );
    }
    else
    {
      int width = (int)g.getClip().getBounds().getWidth();
      int height = (int)g.getClip().getBounds().getHeight();
      updateImage( width, height, bbox );
    }
  }

  public void updateImage( int width, int height, GM_Envelope bbox )
  {
    final HashMap wmsParameter = new HashMap();
    wmsParameter.put( "SERVICE", "WMS" );
    wmsParameter.put( "VERSION", "1.1.1" ); // 1.0.0 ??
    wmsParameter.put( "REQUEST", "getMap" );
    wmsParameter.put( "LAYERS", m_layers );
    wmsParameter.put( "FORMAT", "image/png" );
    wmsParameter.put( "TRANSPARENT", "TRUE" );
    wmsParameter.put( "EXCEPTIONS", "application/vnd.ogc.se_xml" );
    try
    {
      wmsParameter.put( "SRS", m_crs.getName() );
    }
    catch( Exception err )
    {
      err.printStackTrace();
    }

    wmsParameter.put( "WIDTH", "" + width );
    wmsParameter.put( "HEIGHT", "" + height );

    String bboxValue = env2bboxString( bbox );
    wmsParameter.put( "BBOX", bboxValue );

    try
    {
      if( my_requestBBox != null && my_requestBBox.equals( bbox ) )
        return;
      
      final String id = "KalypsoWMSRequest" + getName() + ( new Date() ).toString();
      final OGCWebServiceRequest request = WMSProtocolFactory.createGetMapRequest( id, //java.lang.String
          // id,
          wmsParameter ); //java.util.HashMap model

      OGCWebServiceEvent ogcWSEvent = new OGCWebServiceEvent_Impl( this, //source
          request, //request
          null, //message
          this ); //client
      my_requestBBox = bbox;
      my_requestId = id;
      m_remoteWMS.doService( ogcWSEvent );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  //Helper
  private static String env2bboxString( GM_Envelope env )
  {
    return round( env.getMin().getX() ) + "," + round( env.getMin().getY() ) + ","
        + round( env.getMax().getX() ) + "," + round( env.getMax().getY() );
  }

  //Helper
  private static String round( double value )
  {
    String result = "" + value;
    if( result.length() > 8 )
      return result.substring( 0, 8 );
    return result;
  }

  /**
   * @see org.deegree.services.OGCWebServiceClient#write(java.lang.Object)
   */
  public void write( final Object result )
  {
    if( result instanceof OGCWebServiceEvent )
    {
      OGCWebServiceResponse response = ( (OGCWebServiceEvent)result ).getResponse();

      if( response instanceof WMSGetMapResponse )
      {
        // is it the response to the last request ?
        if( !my_requestId.equals( ( (WMSGetMapResponse)response ).getRequest().getId() ) )
          return;

        Object map = ( (WMSGetMapResponse)response ).getMap();

        if( map != null && map instanceof Image )
        {
          myImage = (Image)map;
          myEnv = my_requestBBox;

          fireModellEvent( null );
        }
        else
        {
          final Document wmsException = ( (WMSGetMapResponse)response ).getException();
          if( wmsException != null )
            System.out.println( "OGC_WMS_Exception:" + XMLHelper.toString( wmsException ) );
        }
      }
    }
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#dispose()
   */
  public void dispose()
  {
  //  
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getBoundingBox()
   */
  public GM_Envelope getBoundingBox()
  {
    return null;
  }
}