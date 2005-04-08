/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

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

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
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

import javax.naming.OperationNotSupportedException;

import org.deegree.services.OGCWebServiceClient;
import org.deegree.services.OGCWebServiceEvent;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.OGCWebServiceResponse;
import org.deegree.services.WebServiceException;
import org.deegree.services.wms.capabilities.WMSCapabilities;
import org.deegree.services.wms.protocol.WMSGetMapResponse;
import org.deegree.xml.XMLParsingException;
import org.deegree_impl.services.OGCWebServiceEvent_Impl;
import org.deegree_impl.services.wms.RemoteWMService;
import org.deegree_impl.services.wms.capabilities.OGCWMSCapabilitiesFactory;
import org.deegree_impl.services.wms.protocol.WMSProtocolFactory;
import org.kalypso.java.util.PropertiesHelper;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.gml.schema.XMLHelper;
import org.kalypsodeegree_impl.tools.NetWorker;
import org.kalypsodeegree_impl.tools.WMSHelper;
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

  private CS_CoordinateSystem m_crs = null;

  private RemoteWMService m_remoteWMS;

  private final String m_source;

  private boolean m_authentification;

  private String m_pass;

  private String m_user;

  private GeoTransform m_transformation;

  private GM_Envelope m_maxEnv = null;

  public KalypsoWMSTheme( final String themeName, final String source,
      final CS_CoordinateSystem localCRS )
  {
    super( themeName );

    m_source = source;

    final Properties sourceProps = PropertiesHelper.parseFromString( source, '#' );

    m_layers = sourceProps.getProperty( "LAYERS", "" );

    final String service = sourceProps.getProperty( "URL", "" );

    // TODO: maybe do this in a thread
    try
    {
      final OGCWMSCapabilitiesFactory wmsCapFac = new OGCWMSCapabilitiesFactory();

      final URL url = new URL( service + "?SERVICE=WMS&VERSION=1.1.1&REQUEST=GetCapabilities" );

      final URLConnection c = url.openConnection();
      NetWorker.configureProxy( c );
      // checks authentification TODO test if it works (this is a fast
      // implemention)
      //      if( NetWorker.requiresAuthentification(c) )
      //      
      //      {
      //        setAuthentification( true );
      //        m_pass = sourceProps.getProperty("PASS", null);
      //        m_user = sourceProps.getProperty("USER", null);
      //        if(m_pass == null || m_user == null )
      //          return;
      //        final String pw = m_user + ":" + m_pass;
      //        final String epw = "Basic " + ( new BASE64Encoder() ).encode(
      // pw.getBytes() );
      //
      //        c.addRequestProperty( "Proxy-Authorization", epw );
      //      }

      c.addRequestProperty( "SERVICE", "WMS" );
      c.addRequestProperty( "VERSION", "1.1.1" );
      c.addRequestProperty( "REQUEST", "GetCapabilities" );
      final Reader reader = new InputStreamReader( c.getInputStream() );

      final WMSCapabilities wmsCaps = wmsCapFac.createCapabilities( reader );
      m_remoteWMS = new RemoteWMService( wmsCaps );

      CS_CoordinateSystem[] crs = WMSHelper.negotiateCRS( localCRS, wmsCaps, m_layers.split( "," ) );
      if( crs.length > 1 )
      {
        GeoTransform gt = findGeotransformation();
        if( gt == null )
          throw new OperationNotSupportedException(
              "No coordinate transformation possible for this Layer" );
        m_transformation = gt;
      }
      else
        m_crs = crs[0];
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
    catch( OperationNotSupportedException e )
    {
      e.printStackTrace();
      //TODO what is to do ??
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  private void setAuthentification( boolean b )
  {
    m_authentification = b;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paintSelected(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, double,
   *      org.kalypsodeegree.model.geometry.GM_Envelope, int)
   */
  public void paintSelected( Graphics g, GeoTransform p, double scale, GM_Envelope bbox,
      int selectionId )
  {
    // kann keine selektion zeichnen!
    if( selectionId != 0 )
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

    //    if( m_authentification )
    //    {
    //      if(m_pass == null || m_user == null )
    //        return;
    //      final String pw = m_user + ":" + m_pass;
    //      final String epw = "Basic " + ( new BASE64Encoder() ).encode(
    // pw.getBytes() );
    //
    //      wmsParameter.put("Proxy-Authorization", epw );
    //    }

    try
    {
      //null pointer exception
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
          {
            // TODO create empty image and paint exception message on it
            //            XMLTools.getStringValue(wmsException);
            //             System.out.println( "OGC_WMS_Exception:" + XMLTools.);
            System.out.println( "OGC_WMS_Exception:" + XMLHelper.toString( wmsException ) );
          }
        }
      }
    }
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#dispose()
   */
  public void dispose()
  {
    //do nothing (no graphics to dispose)
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getBoundingBox()
   */
  public GM_Envelope getBoundingBox()
  {
    return m_maxEnv;
  }

  /**
   * Returns the Transfomed image Frage: muss dies ein GeoTransform von Deegree
   * oder kalypsodeegree sein ?? -> wäre besser wenn hier deegree verwendet
   * würde und nich kalypsodeegree
   */
  private GeoTransform findGeotransformation() throws OperationNotSupportedException
  {
    //TODO find transformable CRS and return Transformation Parameters ??
    throw new OperationNotSupportedException( "The Server can not find a matching Coordinate"
        + "Reference System (CRS)\n. The transformation into the local CRS (requested) is not\n"
        + "implemented yet" );
  }

  //    }
  //    String[] wmsCRS = layer.getSrs();
  //    for( int i = 0; i < wmsCRS.length; i++ )
  //    {
  //      CS_CoordinateSystem cs = ConvenienceCSFactory.getInstance().getOGCCSByName(
  // wmsCRS[i] );
  //
  //      if( cs != null && cs.equals( localCRS ) )
  //        m_crs = localCRS;
  //      else if( cs != null )
  //      {
  //        //TODO create a CRS Transformation
  //        //
  // CoordinateTransformationFactory.getDefault().createFromCoordinateSystems(cs,
  //        // localCRS);
  //      }
  //
  //    }
  //    Layer[] layers = layer.getLayer();
  //    for( int j = 0; j < layers.length; j++ )
  //    {
  //      Layer insideLayer = layers[j];
  //      String[] layersCSR = insideLayer.getSrs();
  //      for( int i = 0; i < layersCSR.length; i++ )
  //      {
  //        CS_CoordinateSystem layerCSR =
  // ConvenienceCSFactory.getInstance().getOGCCSByName(
  //            layersCSR[i] );
  //
  //        if( layerCSR != null && layerCSR.getName().equals( localCRS.getName() ) )
  //          m_crs = layerCSR;
  //      }
  //
  //    }
  //  }
  public String getSource()
  {
    return m_source;
  }

  public boolean contains( String[] srs, String match )
  {
    for( int i = 0; i < srs.length; i++ )
    {
      if( srs[i].equals( match ) )
        return true;
    }
    return false;
  }

}// class KalypsoWMSTheme
