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
import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Date;
import java.util.HashMap;
import java.util.Properties;

import javax.media.jai.PlanarImage;
import javax.media.jai.TiledImage;
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
import org.kalypsodeegree_impl.model.ct.GeoTransformer;
import org.kalypsodeegree_impl.tools.NetWorker;
import org.kalypsodeegree_impl.tools.WMSHelper;
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Document;

/**
 * @author Kuepferle
 */
public class KalypsoWMSTheme extends AbstractKalypsoTheme implements OGCWebServiceClient
{
  private final String m_layers;

  private TiledImage m_remoteImage = null;

  private String m_source = null;

  private String my_requestId = null;

  private GM_Envelope m_requestedBBox = null;

  private CS_CoordinateSystem m_localCSR = null;

  private RemoteWMService m_remoteWMS;

  private CS_CoordinateSystem m_remoteCSR = null;

  private boolean m_authentification;

  private String m_pass;

  private String m_user;

  private GM_Envelope m_maxEnv = null;

  public KalypsoWMSTheme( final String linktype, final String themeName, final String source, final CS_CoordinateSystem localCRS )
  {
    super( themeName, linktype.toUpperCase() );
    final Properties sourceProps = PropertiesHelper.parseFromString( source, '#' );
    m_layers = sourceProps.getProperty( "LAYERS", "" );
    final String service = sourceProps.getProperty( "URL", "" );
    m_localCSR = localCRS;
    m_source = source;

    // TODO: maybe do this in a thread
    try
    {
      final OGCWMSCapabilitiesFactory wmsCapFac = new OGCWMSCapabilitiesFactory();

      final URL url = new URL( service + "?SERVICE=WMS&VERSION=1.1.1&REQUEST=GetCapabilities" );

      final URLConnection c = url.openConnection();
      NetWorker.configureProxy( c );
      c.addRequestProperty( "SERVICE", "WMS" );
      c.addRequestProperty( "VERSION", "1.1.1" );
      c.addRequestProperty( "REQUEST", "GetCapabilities" );
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

      //create capabilites from the request
      final Reader reader = new InputStreamReader( c.getInputStream() );
      final WMSCapabilities wmsCaps = wmsCapFac.createCapabilities( reader );
      m_remoteWMS = new RemoteWMService( wmsCaps );
      //match the local with the remote coordiante system
      CS_CoordinateSystem[] crs = WMSHelper.negotiateCRS( m_localCSR, wmsCaps, m_layers.split( "," ) );
      if( !crs[0].equals( m_localCSR ) )
        m_remoteCSR = crs[0];
      else
        m_remoteCSR = m_localCSR;
      //set max extent for Map Layer
      m_maxEnv = WMSHelper.getMaxExtend( m_layers.split( "," ), wmsCaps, m_remoteCSR );
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
   * @param g the graphics context from the map panel
   * @param p world to screen transformation
   * @param scale scale
   * @param bbox bounding box from map model (screen)
   * 
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paintSelected(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, double,
   *      org.kalypsodeegree.model.geometry.GM_Envelope, int)
   */
  public void paintSelected( Graphics g, GeoTransform p, double scale, GM_Envelope bbox, int selectionId )
  {
    // kann keine selektion zeichnen!
    if( selectionId != 0 )
      return;
    //the image is only updated when the wish bbox is ok
    if( m_requestedBBox != null && m_requestedBBox.equals( bbox ) && m_remoteImage != null )
    {

      GM_Envelope remoteEnv = null;
      try
      {
        GeoTransformer gt = new GeoTransformer( m_remoteCSR );
        remoteEnv = gt.transformEnvelope( m_requestedBBox, m_localCSR );
        WMSHelper.transformImage( m_remoteImage, remoteEnv, m_localCSR, m_remoteCSR, p, g );
        //      g.setPaintMode();
        //      g.drawImage( m_remoteImage, 0, 0, null );
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }

    else
    {
      int width = (int)g.getClip().getBounds().getWidth();
      int height = (int)g.getClip().getBounds().getHeight();
      updateImage( width, height, bbox );
    }
  }

  public void paintSelected( Graphics g, Graphics hg, GeoTransform p, double scale, GM_Envelope bbox, int selectionId )
  {
    paintSelected( hg, p, scale, bbox, selectionId );
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

    GM_Envelope remoteEnv = null;
    try
    {
      //null pointer exception
      wmsParameter.put( "SRS", m_remoteCSR.getName() );
      GeoTransformer gt = new GeoTransformer( m_remoteCSR );
      remoteEnv = gt.transformEnvelope( bbox, m_localCSR );
    }
    catch( Exception err )
    {
      err.printStackTrace();
    }

    wmsParameter.put( "WIDTH", "" + width );
    wmsParameter.put( "HEIGHT", "" + height );

    //    String bboxValue = env2bboxString( bbox );
    String bboxValue = env2bboxString( remoteEnv );
    wmsParameter.put( "BBOX", bboxValue );

    try
    {
      if( m_requestedBBox != null && m_requestedBBox.equals( bbox ) )
        return;

      final String id = "KalypsoWMSRequest" + getName() + ( new Date() ).toString();
      final OGCWebServiceRequest request = WMSProtocolFactory.createGetMapRequest( id, //java.lang.String
          // id,
          wmsParameter ); //java.util.HashMap model

      OGCWebServiceEvent ogcWSEvent = new OGCWebServiceEvent_Impl( this, //source
          request, //request
          null, //message
          this ); //client
      m_requestedBBox = bbox;
      //      my_requestBBox = remoteEnv;
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
    return round( env.getMin().getX() ) + "," + round( env.getMin().getY() ) + "," + round( env.getMax().getX() ) + "," + round( env.getMax().getY() );
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
          Image image = (Image)map;
          PlanarImage remoteImage = PlanarImage.wrapRenderedImage( (RenderedImage)image );
          m_remoteImage = new TiledImage( remoteImage, true );

          fireModellEvent( null );
        }
        else
        {
          final Document wmsException = ( (WMSGetMapResponse)response ).getException();
          if( wmsException != null )
          {
            System.out.println( "OGC_WMS_Exception:\n" + XMLHelper.toString( wmsException ) );
            // TODO @christoph: hier wir ein image mit der Gauss-Krüger BBOX
            // aufgemacht, das gibt sofort eine out of memory exception

            //            int width = (int)m_requestedBBox.getWidth();
            //            int height = (int)m_requestedBBox.getHeight();
            //
            //            final BufferedImage image = new BufferedImage( width, height,
            // BufferedImage.TYPE_INT_ARGB );
            //            final Graphics gr = image.getGraphics();
            //            gr.drawString( "OGC_WMS_Exception:\n" + XMLHelper.toString(
            // wmsException ), width / 2, height / 2 );
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
    GM_Envelope bbox = null;
    try
    {
      GeoTransformer gt = new GeoTransformer( m_localCSR );
      bbox = gt.transformEnvelope( m_maxEnv, m_remoteCSR );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    return bbox;

  }

  public String getSource()
  {
    return m_source;
  }

  //  public void saveTheme( IProgressMonitor monitor )
  //  {
  //   

  //    
  //    try
  //    {
  //      ImageOutputStream ios = ImageIO.createImageOutputStream( new File(
  // "c:/temp" ) );
  //      ImageIO.write( (RenderedImage)m_remoteImage, m_layers, ios );
  //    }
  //    catch( IOException e )
  //    {
  //      // TODO: handle exception
  //    }
  //    
  //  }

}// class KalypsoWMSTheme
