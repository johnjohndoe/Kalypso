package org.kalypso.ogc.gml;

import java.awt.Graphics;
import java.awt.Image;
import java.util.Date;
import java.util.HashMap;

import org.deegree.graphics.sld.UserStyle;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.services.OGCWebServiceClient;
import org.deegree.services.OGCWebServiceEvent;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.OGCWebServiceResponse;
import org.deegree.services.wms.protocol.WMSGetMapResponse;
import org.deegree_impl.services.OGCWebServiceEvent_Impl;
import org.deegree_impl.services.wms.protocol.WMSProtocolFactory;
import org.kalypso.util.xml.XMLTools;
import org.w3c.dom.Document;

/**
 * @author doemming
 */
public class KalypsoWMSTheme extends AbstractKalypsoTheme implements OGCWebServiceClient
{
  private final KalypsoWMSLayer myLayer;

  private Image myImage = null;

  private GM_Envelope myEnv = null;

  private String my_requestId = null;

  private GM_Envelope my_requestBBox = null;

  public KalypsoWMSTheme( final String name,final KalypsoWMSLayer layer )
  {
    super( name );
    myLayer = layer;

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
    wmsParameter.put( "LAYERS", myLayer.getLayerList() );
    wmsParameter.put( "FORMAT", "image/png" );
    wmsParameter.put( "TRANSPARENT", "TRUE" );
    wmsParameter.put( "EXCEPTIONS", "application/vnd.ogc.se_xml" );
    try
    {
      wmsParameter.put( "SRS", myLayer.getCoordinatesSystem().getName() );
      System.out.println( "SRS:" + myLayer.getCoordinatesSystem().getName() );
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
      String id = "KalypsoWMSRequest" + myLayer.getName() + ( new Date() ).toString();
      OGCWebServiceRequest request = WMSProtocolFactory.createGetMapRequest( id, //java.lang.String
          // id,
          wmsParameter ); //java.util.HashMap model

      OGCWebServiceEvent ogcWSEvent = new OGCWebServiceEvent_Impl( this, //source
          request, //request
          null, //message
          this ); //client
      my_requestBBox = bbox;
      my_requestId = id;
      myLayer.getRemoteWMService().doService( ogcWSEvent );
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
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getStyles()
   */
  public UserStyle[] getStyles()
  {
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#addStyle(org.kalypso.ogc.gml.KalypsoUserStyle)
   */
  public void addStyle( KalypsoUserStyle style )
  {
  // TODO
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#removeStyle(org.kalypso.ogc.gml.KalypsoUserStyle)
   */
  public void removeStyle( KalypsoUserStyle style )
  {
  // TODO
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getLayer()
   */
  public IKalypsoLayer getLayer()
  {
    return myLayer;
  }

  /**
   * @see org.deegree.services.OGCWebServiceClient#write(java.lang.Object)
   */
  public void write( Object result )
  {
    System.out.println( "\n\n\n got ogcwebservicerespons: " + result.getClass().toString() );

    if( result instanceof OGCWebServiceEvent )
    {
      OGCWebServiceResponse response = ( (OGCWebServiceEvent)result ).getResponse();
      System.out.println( "response is " + response.getClass().toString() );

      if( response instanceof WMSGetMapResponse )
      {
        // is it the response to the last request ?
        if( !my_requestId.equals( ( (WMSGetMapResponse)response ).getRequest().getId() ) )
          return;

        Object map = ( (WMSGetMapResponse)response ).getMap();
        System.out.println( "ResponseID:" + ( (WMSGetMapResponse)response ).getRequest().getId() );

        if( map != null && map instanceof Image )
        {
          myImage = (Image)map;
          myEnv = my_requestBBox;
          fireModellEvent( null );
        }
        else
        {
          System.out.println( "no Image :-(" );
          final Document wmsException = ( (WMSGetMapResponse)response ).getException();
          if( wmsException != null )
            System.out.println( XMLTools.toString( wmsException ) );
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
}