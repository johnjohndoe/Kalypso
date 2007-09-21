/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.map.themes.provider;

import java.awt.Font;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Date;
import java.util.HashMap;
import java.util.Properties;

import javax.imageio.ImageIO;

import org.deegree.ogcwebservices.OGCWebServiceException;
import org.deegree.ogcwebservices.OGCWebServiceRequest;
import org.deegree.ogcwebservices.wms.capabilities.Layer;
import org.deegree.ogcwebservices.wms.capabilities.LegendURL;
import org.deegree.ogcwebservices.wms.capabilities.Style;
import org.deegree.ogcwebservices.wms.capabilities.WMSCapabilities;
import org.deegree.ogcwebservices.wms.operation.GetMap;
import org.deegree.ogcwebservices.wms.operation.GetMapResult;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.commons.java.util.PropertiesHelper;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.ogc.gml.map.themes.KalypsoRemoteWMService;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * This provider loads an image from a WMS. It caches the capabilities, so that this request is only done once.
 * 
 * @author Holger Albert
 */
public class KalypsoWMSImageProvider implements IKalypsoImageProvider, IKalypsoLegendProvider
{
  /**
   * This constant stores the key for the layers.
   */
  public final static String KEY_LAYERS = "LAYERS";

  /**
   * This constant stores the key for the URL.
   */
  public final static String KEY_URL = "URL";

  /**
   * This constant stores the key for the styles.
   */
  public final static String KEY_STYLES = "STYLES";

  /**
   * This constant stores the type name.
   */
  public final static String TYPE_NAME = "wms";

  /**
   * This variable stores the name of the theme.
   */
  private String m_themeName;

  /**
   * This variable stores some parameters for the WMS.
   */
  private String m_source;

  /**
   * This variable stores the client coordinate system.
   */
  private CS_CoordinateSystem m_localSRS;

  /**
   * This variable stores the WMS service or is null.
   */
  private KalypsoRemoteWMService m_wms;

  /**
   * The constructor.
   * 
   * @param themeName
   *            The name of the theme.
   * @param source
   *            Some parameters for the WMS.
   * @param localSRS
   *            The client coordinate system.
   */
  public KalypsoWMSImageProvider( String themeName, String source, CS_CoordinateSystem localSRS )
  {
    m_themeName = themeName;
    m_source = source;
    m_localSRS = localSRS;

    m_wms = null;
  }

  /**
   * @see org.kalypso.ogc.gml.map.themes.provider.IKalypsoImageProvider#getImage(double, double,
   *      org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  public Image getImage( int width, int height, GM_Envelope bbox ) throws CoreException
  {
    /* Initialize the remote WMS, if it is not already done. */
    initializeRemoteWMS();

    /* Load the new image. */
    Image result = loadImage( width, height, bbox );

    return result;
  }

  /**
   * @see org.kalypso.ogc.gml.map.themes.provider.IKalypsoLegendProvider#getLegendGraphic(java.awt.Font,
   *      java.lang.String)
   */
  public Image getLegendGraphic( Font font, String layerName ) throws CoreException
  {
    /* Initialize the remote WMS, if it is not already done. */
    initializeRemoteWMS();

    /* Load the legend. */
    Image result = loadLegendGraphic( layerName );

    return result;
  }

  /**
   * This function initializes the WMS and loads the Capabilities.
   */
  private void initializeRemoteWMS( ) throws CoreException
  {
    /* Cache the WMS, so that its capabilities are only initialized once. */
    if( m_wms == null )
    {
      /* Parse the source into properties. */
      Properties sourceProps = PropertiesHelper.parseFromString( m_source, '#' );
      String layers = sourceProps.getProperty( KEY_LAYERS, null );
      String styles = sourceProps.getProperty( KEY_STYLES, null );
      String service = sourceProps.getProperty( KEY_URL, null );

      /* Create the service URL. */
      URL serviceURL = parseServiceUrl( service );

      /* Initialize the WMS. */
      m_wms = KalypsoRemoteWMService.initializeService( serviceURL, m_localSRS, layers, styles, new NullProgressMonitor() );
    }
  }

  /**
   * This function parses a String into an URL to the WMS service.
   * 
   * @param service
   *            The String representation of the URL to the WMS service.
   * @return The URL to the WMS service.
   */
  private URL parseServiceUrl( String service ) throws CoreException
  {
    try
    {
      return new URL( service );
    }
    catch( MalformedURLException e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e, String.format( "Service URL fehlerhaft: %s (%s)", service, e.getLocalizedMessage() ) ) );
    }
  }

  /**
   * This function loads the image. If there could be image retrieved, it will return null. If an error occures, it will
   * throw an Exception.
   * 
   * @param width
   *            The requested width.
   * @param height
   *            The requested height.
   * @param bbox
   *            The requested bounding box.
   * @return The loaded image or null.
   */
  private Image loadImage( int width, int height, GM_Envelope bbox ) throws CoreException
  {
    /* Check if nothing to request. */
    if( bbox == null )
      return null;

    /* If the some of the parameters has the wrong values, return null. */
    if( width == 0 || height == 0 || bbox.getWidth() == 0 || bbox.getHeight() == 0 )
      return null;

    /* Work locally against a copy of the reference, because it may change any time... */
    KalypsoRemoteWMService remoteWMS = m_wms;
    if( remoteWMS == null )
      return null;

    /* Create the parameters for the request. */
    HashMap<String, String> parameterMap = remoteWMS.createGetMapRequestParameter( width, height, bbox, m_localSRS );

    /* Add the ID parameter to them. */
    parameterMap.put( "ID", "KalypsoWMSRequest" + m_themeName + new Date().getTime() );

    try
    {
      /* Create the GetMap request. */
      GetMap request = GetMap.create( parameterMap );

      /* Do the request and wait, until the result is there. */
      Object result = remoteWMS.doService( request );

      /* No result, no image is returned. */
      if( result == null )
        return null;

      /* Wrong result, no image is returned. */
      if( !(result instanceof GetMapResult) )
        return null;

      /* Cast. */
      GetMapResult mapResponse = (GetMapResult) result;

      /* Get the image. */
      Image resultImage = (Image) mapResponse.getMap();
      if( resultImage == null )
      {
        /* Handle service-exception: convert to status and set it. */
        OGCWebServiceRequest mapRequest = mapResponse.getRequest();
        OGCWebServiceException exception = mapResponse.getException();

        MultiStatus status = new MultiStatus( KalypsoCorePlugin.getID(), 0, "Zugriffsfehler", null );
        status.add( StatusUtilities.createErrorStatus( "Request: '" + mapRequest + "'" ) );
        status.add( StatusUtilities.createErrorStatus( "Exception-Dokument: " ) );
        status.add( StatusUtilities.createMultiStatusFromMessage( IStatus.ERROR, KalypsoCorePlugin.getID(), 0, exception.toString(), "\n", null ) );

        throw new CoreException( status );
      }

      return resultImage;
    }
    catch( Throwable t )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( t ) );
    }
  }

  /**
   * This function loads the legend graphic.
   * 
   * @param layerName
   *            The name of the layer, for which the legend should be loaded.
   * @return The legend graphic.
   */
  private Image loadLegendGraphic( String layerName ) throws CoreException
  {
    /* We need a remote WMS. */
    if( m_wms == null )
      return null;

    /* Check, if there is a legend graphic available. */
    WMSCapabilities capabilities = m_wms.getWMSCapabilities();

    /* All layers of the theme. */
    Layer[] layers = capabilities.getLayer().getLayer();

    /* No layers, no legend graphic. */
    if( layers.length == 0 )
      return null;

    /*
     * TODO: Only the first layer will be used, for now. Except, if there is a layer with the same title as provided.
     * But this is not good.
     */
    Layer layer = layers[0];
    for( int i = 0; i < layers.length; i++ )
    {
      if( layers[i].getTitle().equals( layerName ) )
      {
        layer = layers[i];
        break;
      }
    }

    /* Get all styles. */
    Style[] styles = layer.getStyles();

    /* No styles, no legend graphic. */
    if( styles.length == 0 )
      return null;

    /* TODO: Only the first style will be used, for now. */
    Style style = styles[0];

    /* Get the URLs for the legend. */
    LegendURL[] legendURLs = style.getLegendURL();

    /* No legend URLs, no legend. */
    if( legendURLs.length == 0 )
      return null;

    /* TODO: Only the first legend URL will be used for now. */
    LegendURL legendURL = legendURLs[0];

    /* Get the real URL. */
    URL onlineResource = legendURL.getOnlineResource();

    /* The result image. */
    BufferedImage result = null;

    try
    {
      /* Load the image. */
      result = ImageIO.read( onlineResource );
    }
    catch( Exception e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }

    return result;
  }

  /**
   * @see org.kalypso.ogc.gml.map.themes.provider.IKalypsoImageProvider#getLabel()
   */
  public String getLabel( )
  {
    return "WMS Thema: " + m_source;
  }

  /**
   * This function returns the href of the request.
   * 
   * @return The href of the request.
   */
  public String getSource( )
  {
    return m_source;
  }

  /**
   * @see org.kalypso.ogc.gml.map.themes.provider.IKalypsoImageProvider#getFullExtent()
   */
  public GM_Envelope getFullExtent( )
  {
    GM_Envelope maxExtent = null;

    try
    {
      maxExtent = m_wms.getMaxExtend( m_localSRS );
    }
    catch( CoreException e )
    {
      // do nothing
    }

    return maxExtent;
  }
}