/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.ogc.gml.wms.provider.images;

import java.awt.Image;
import java.net.MalformedURLException;
import java.net.URL;
import java.rmi.RemoteException;

import org.deegree.ogcwebservices.wms.RemoteWMService;
import org.deegree.ogcwebservices.wms.capabilities.WMSCapabilities;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.swt.graphics.Font;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.ogc.gml.wms.deegree.DeegreeWMSUtilities;
import org.kalypso.ogc.gml.wms.loader.ICapabilitiesLoader;
import org.kalypso.ogc.gml.wms.provider.legends.IKalypsoLegendProvider;
import org.kalypso.transformation.GeoTransformer;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * The base implementation of the deegree WMS client.
 * 
 * @author Holger Albert
 */
public abstract class AbstractDeegreeImageProvider implements IKalypsoImageProvider, IKalypsoLegendProvider
{
  /**
   * This variable stores the name of the theme.
   */
  private String m_themeName;

  /**
   * The LAYERS property of the source.
   */
  private String m_layers;

  /**
   * The STYLES property of the source.
   */
  private String m_styles;

  /**
   * The service.
   */
  private String m_service;

  /**
   * This variable stores the client coordinate system.
   */
  private String m_localSRS;

  /**
   * This variable stores the loader for the capabilities.
   */
  private ICapabilitiesLoader m_loader;

  /**
   * This variable stores the WMS service or is null.
   */
  private RemoteWMService m_wms;

  /**
   * The negotiated coordinate system.
   */
  private String m_negotiatedSRS;

  /**
   * The constructor.
   * 
   * @param loader
   *            The loader for loading the capabilities.
   */
  public AbstractDeegreeImageProvider( ICapabilitiesLoader loader )
  {
    m_themeName = null;
    m_layers = null;
    m_styles = null;
    m_service = null;
    m_localSRS = null;

    m_loader = loader;
    m_wms = null;
    m_negotiatedSRS = null;
  }

  /**
   * @see org.kalypso.ogc.gml.wms.provider.images.IKalypsoImageProvider#init(java.lang.String, java.lang.String,
   *      java.lang.String, java.lang.String, java.lang.String)
   */
  public void init( String themeName, String layers, String styles, String service, String localSRS )
  {
    m_themeName = themeName;
    m_layers = layers;
    m_styles = styles;
    m_service = service;
    m_localSRS = localSRS;

    m_wms = null;
    m_negotiatedSRS = null;
  }

  /**
   * @see org.kalypso.ogc.gml.wms.provider.IKalypsoImageProvider#getImage(int, int,
   *      org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  public Image getImage( int width, int height, GM_Envelope bbox ) throws CoreException
  {
    /* Initialize the remote WMS, if it is not already done. */
    initializeRemoteWMS();

    return loadImage( width, height, bbox );
  }

  /**
   * @see org.kalypso.ogc.gml.wms.provider.IKalypsoLegendProvider#getLegendGraphic(org.eclipse.swt.graphics.Font)
   */
  public org.eclipse.swt.graphics.Image getLegendGraphic( Font font ) throws CoreException
  {
    /* Initialize the remote WMS, if it is not already done. */
    initializeRemoteWMS();

    /* Load the legend. */
    org.eclipse.swt.graphics.Image result = loadLegendGraphic( "" );

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
      if( m_service == null )
        throw new CoreException( StatusUtilities.createErrorStatus( "No service url given ..." ) );

      /* Create the service URL. */
      URL serviceURL = parseServiceUrl( m_service );

      /* Init the loader. */
      m_loader.init( serviceURL );

      /* Create the capabilities. */
      WMSCapabilities wmsCaps = DeegreeWMSUtilities.loadCapabilities( m_loader, new NullProgressMonitor() );

      /* Ask for the srs. */
      m_negotiatedSRS = negotiateCRS( m_localSRS, wmsCaps, m_layers.split( "," ) );

      /* Initialize the WMS. */
      m_wms = getRemoteService( wmsCaps );
    }
  }

  /**
   * This function creates the remote service and returns it.
   * 
   * @param capabilities
   *            The capabilites for the remote service.
   * @return The remote service.
   */
  protected abstract RemoteWMService getRemoteService( WMSCapabilities capabilities );

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
   * This method tries to find a common spatial reference system (srs) for a given set of layers. If all layers
   * coorespond to the local crs the local crs is returned, otherwise the srs of the top layer is returned and the
   * client must choose one to transform it to the local coordinate system
   * 
   * @param localCRS
   *            The local spatial reference system.
   * @param capabilities
   *            The capabilites document of the web map service.
   * @param layerNames
   *            The layers that have to be matched to the local srs.
   * @return An array of possible coordiante systems.
   */
  private String negotiateCRS( String localSRS, WMSCapabilities wmsCapabilities, String[] layers ) throws CoreException
  {
    /* Match the local with the remote coordinate system. */
    try
    {
      String[] crs = DeegreeWMSUtilities.negotiateCRS( localSRS, wmsCapabilities, layers );
      if( crs.length > 0 )
        return crs[0];
    }
    catch( RemoteException e )
    {
      /* Create the error status. */
      IStatus errorStatus = StatusUtilities.createErrorStatus( "Failed to negotiate CRS." );

      /* Log the error. */
      KalypsoGisPlugin.getDefault().getLog().log( errorStatus );

      throw new CoreException( errorStatus );
    }

    return localSRS;
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
  protected abstract Image loadImage( int width, int height, GM_Envelope bbox ) throws CoreException;

  /**
   * This function loads the legend graphic.
   * 
   * @param layerName
   *            The name of the layer, for which the legend should be loaded.
   * @return The legend graphic.
   */
  protected abstract org.eclipse.swt.graphics.Image loadLegendGraphic( String layerName ) throws CoreException;

  /**
   * @see org.kalypso.ogc.gml.map.themes.provider.IKalypsoImageProvider#getFullExtent()
   */
  public GM_Envelope getFullExtent( )
  {
    try
    {
      /* Initialize the remote WMS, if it is not already done. */
      initializeRemoteWMS();

      if( m_wms == null || m_layers == null )
        return null;

      GM_Envelope maxEnvRemoteSRS = DeegreeWMSUtilities.getMaxExtent( m_layers.split( "," ), (WMSCapabilities) m_wms.getCapabilities(), m_negotiatedSRS );
      GeoTransformer gt = new GeoTransformer( m_localSRS );

      return gt.transformEnvelope( maxEnvRemoteSRS, m_negotiatedSRS );
    }
    catch( Exception ex )
    {
      KalypsoGisPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( ex, "Failed to determine extent." ) );
    }

    return null;
  }

  protected String getThemeName( )
  {
    return m_themeName;
  }

  protected String getLayers( )
  {
    return m_layers;
  }

  protected String getStyles( )
  {
    return m_styles;
  }

  protected String getService( )
  {
    return m_service;
  }

  protected String getLocalSRS( )
  {
    return m_localSRS;
  }

  /**
   * @see org.kalypso.ogc.gml.wms.provider.IKalypsoImageProvider#getLoader()
   */
  public ICapabilitiesLoader getLoader( )
  {
    return m_loader;
  }

  protected RemoteWMService getWms( )
  {
    return m_wms;
  }

  protected String getNegotiatedSRS( )
  {
    return m_negotiatedSRS;
  }
}