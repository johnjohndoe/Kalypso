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

import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author doemming
 */
public class KalypsoWMSTheme extends AbstractKalypsoTheme //implements
// OGCWebServiceClient
{

  // auskommentiert bis deegreebackup aufgeraemt ist

  //  private final String m_layers;
  //
  //  private Image myImage = null;
  //
  //  private GM_Envelope myEnv = null;
  //
  //  private String my_requestId = null;
  //
  //  private GM_Envelope my_requestBBox = null;
  //
  //  private final CS_CoordinateSystem m_crs;
  //
  //  private RemoteWMService m_remoteWMS;
  //
  //  public KalypsoWMSTheme( final String themeName, final String source, final
  // CS_CoordinateSystem crs )
  //  {
  //    super( themeName );
  //
  //    final Properties sourceProps = PropertiesHelper.parseFromString( source,
  // '#' );
  //    
  //    m_crs = crs;
  //    m_layers = sourceProps.getProperty( "LAYERS", "" );
  //    
  //    final String service = sourceProps.getProperty( "SERVICE", "" );
  //
  //    // TODO: maybe do this in a thread
  //    try
  //    {
  //      final OGCWMSCapabilitiesFactory wmsCapFac = new
  // OGCWMSCapabilitiesFactory();
  //
  //      final URL url = new URL( service +
  // "SERVICE=WMS&VERSION=1.1.1&REQUEST=GetCapabilities" );
  //
  //      final URLConnection c = url.openConnection();
  //      NetWorker.configureProxy( c );
  //      
  //      c.addRequestProperty( "SERVICE", "WMS" );
  //      c.addRequestProperty( "VERSION", "1.1.1" );
  //      c.addRequestProperty( "REQUEST", "GetCapabilities" );
  //      final Reader reader = new InputStreamReader( c.getInputStream() );
  //
  //      final WMSCapabilities wmsCaps = wmsCapFac.createCapabilities( reader );
  //      m_remoteWMS = new RemoteWMService( wmsCaps );
  //    }
  //    catch( MalformedURLException e )
  //    {
  //      e.printStackTrace();
  //    }
  //    catch( IOException e )
  //    {
  //      e.printStackTrace();
  //    }
  //    catch( XMLParsingException e )
  //    {
  //      e.printStackTrace();
  //    }
  //    catch( WebServiceException e )
  //    {
  //      e.printStackTrace();
  //    }
  //  }
  //
  //  /**
  //   * @see org.kalypso.ogc.gml.IKalypsoTheme#paintSelected(java.awt.Graphics,
  //   * org.kalypsodeegree.graphics.transformation.GeoTransform, double,
  //   * org.kalypsodeegree.model.geometry.GM_Envelope, int)
  //   */
  //  public void paintSelected( Graphics g, GeoTransform p, double scale,
  // GM_Envelope bbox,
  //      int selectionId )
  //  {
  //    // kann keine selektion zeichnen!
  //    if( selectionId != -1 )
  //      return;
  //
  //    if( myEnv != null && myEnv.equals( bbox ) && myImage != null )
  //    {
  //      g.setPaintMode();
  //      g.drawImage( myImage, 0, 0, null );
  //    }
  //    else
  //    {
  //      int width = (int)g.getClip().getBounds().getWidth();
  //      int height = (int)g.getClip().getBounds().getHeight();
  //      updateImage( width, height, bbox );
  //    }
  //  }
  //
  //  public void updateImage( int width, int height, GM_Envelope bbox )
  //  {
  //    final HashMap wmsParameter = new HashMap();
  //    wmsParameter.put( "SERVICE", "WMS" );
  //    wmsParameter.put( "VERSION", "1.1.1" ); // 1.0.0 ??
  //    wmsParameter.put( "REQUEST", "getMap" );
  //    wmsParameter.put( "LAYERS", m_layers );
  //    wmsParameter.put( "FORMAT", "image/png" );
  //    wmsParameter.put( "TRANSPARENT", "TRUE" );
  //    wmsParameter.put( "EXCEPTIONS", "application/vnd.ogc.se_xml" );
  //    try
  //    {
  //      wmsParameter.put( "SRS", m_crs.getName() );
  //    }
  //    catch( Exception err )
  //    {
  //      err.printStackTrace();
  //    }
  //
  //    wmsParameter.put( "WIDTH", "" + width );
  //    wmsParameter.put( "HEIGHT", "" + height );
  //
  //    String bboxValue = env2bboxString( bbox );
  //    wmsParameter.put( "BBOX", bboxValue );
  //
  //    try
  //    {
  //      if( my_requestBBox != null && my_requestBBox.equals( bbox ) )
  //        return;
  //      
  //      final String id = "KalypsoWMSRequest" + getName() + ( new Date()
  // ).toString();
  //      final OGCWebServiceRequest request =
  // WMSProtocolFactory.createGetMapRequest( id, //java.lang.String
  //          // id,
  //          wmsParameter ); //java.util.HashMap model
  //
  //      OGCWebServiceEvent ogcWSEvent = new OGCWebServiceEvent_Impl( this, //source
  //          request, //request
  //          null, //message
  //          this ); //client
  //      my_requestBBox = bbox;
  //      my_requestId = id;
  //      m_remoteWMS.doService( ogcWSEvent );
  //    }
  //    catch( Exception e )
  //    {
  //      e.printStackTrace();
  //    }
  //  }
  //
  //  //Helper
  //  private static String env2bboxString( GM_Envelope env )
  //  {
  //    return round( env.getMin().getX() ) + "," + round( env.getMin().getY() ) +
  // ","
  //        + round( env.getMax().getX() ) + "," + round( env.getMax().getY() );
  //  }
  //
  //  //Helper
  //  private static String round( double value )
  //  {
  //    String result = "" + value;
  //    if( result.length() > 8 )
  //      return result.substring( 0, 8 );
  //    return result;
  //  }
  //
  //  /**
  //   * @see org.kalypsodeegree.services.OGCWebServiceClient#write(java.lang.Object)
  //   */
  //  public void write( final Object result )
  //  {
  //    if( result instanceof OGCWebServiceEvent )
  //    {
  //      OGCWebServiceResponse response = ( (OGCWebServiceEvent)result
  // ).getResponse();
  //
  //      if( response instanceof WMSGetMapResponse )
  //      {
  //        // is it the response to the last request ?
  //        if( !my_requestId.equals( ( (WMSGetMapResponse)response
  // ).getRequest().getId() ) )
  //          return;
  //
  //        Object map = ( (WMSGetMapResponse)response ).getMap();
  //
  //        if( map != null && map instanceof Image )
  //        {
  //          myImage = (Image)map;
  //          myEnv = my_requestBBox;
  //
  //          fireModellEvent( null );
  //        }
  //        else
  //        {
  //          final Document wmsException = ( (WMSGetMapResponse)response
  // ).getException();
  //          if( wmsException != null )
  //            System.out.println( "OGC_WMS_Exception:" + XMLHelper.toString( wmsException
  // ) );
  //        }
  //      }
  //    }
  //  }
  //
  //  /**
  //   * @see org.kalypso.ogc.gml.IKalypsoTheme#dispose()
  //   */
  //  public void dispose()
  //  {
  //  //
  //  }
  //
  //  /**
  //   * @see org.kalypso.ogc.gml.IKalypsoTheme#getBoundingBox()
  //   */
  //  public GM_Envelope getBoundingBox()
  //  {
  //    return null;
  //  }

  public KalypsoWMSTheme( String name, String string, CS_CoordinateSystem system )
  {
    super( name );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#dispose()
   */
  public void dispose()
  {
  // nothing
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#paintSelected(java.awt.Graphics,
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, double,
   *      org.kalypsodeegree.model.geometry.GM_Envelope, int)
   */
  public void paintSelected( Graphics g, GeoTransform p, double scale, GM_Envelope bbox,
      int selectionId )
  {
  // nothing
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoTheme#getBoundingBox()
   */
  public GM_Envelope getBoundingBox()
  {
    return null;
  }

}