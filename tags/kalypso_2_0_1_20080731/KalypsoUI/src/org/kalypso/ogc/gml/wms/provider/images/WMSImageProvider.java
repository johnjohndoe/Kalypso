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
import java.io.IOException;
import java.net.URL;

import org.deegree.ogcwebservices.OGCWebServiceException;
import org.deegree.ogcwebservices.OGCWebServiceRequest;
import org.deegree.ogcwebservices.wms.RemoteWMService;
import org.deegree.ogcwebservices.wms.capabilities.Layer;
import org.deegree.ogcwebservices.wms.capabilities.LegendURL;
import org.deegree.ogcwebservices.wms.capabilities.Style;
import org.deegree.ogcwebservices.wms.capabilities.WMSCapabilities;
import org.deegree.ogcwebservices.wms.operation.GetMap;
import org.deegree.ogcwebservices.wms.operation.GetMapResult;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.swt.widgets.Display;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.wms.deegree.DeegreeWMSUtilities;
import org.kalypso.ogc.gml.wms.loader.WMSCapabilitiesLoader;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * This provider loads an image from a WMS. It caches the capabilities, so that this request is only done once.
 * 
 * @author Holger Albert
 */
public class WMSImageProvider extends AbstractDeegreeImageProvider
{
  /**
   * This constant stores the type name.
   */
  public final static String TYPE_NAME = "wms"; //$NON-NLS-1$

  /**
   * The constructor.
   */
  public WMSImageProvider( )
  {
    super( new WMSCapabilitiesLoader( 3000 ) );
  }

  /**
   * @see org.kalypso.ogc.gml.wms.provider.images.AbstractDeegreeImageProvider#init(java.lang.String, java.lang.String,
   *      java.lang.String, java.lang.String, java.lang.String)
   */
  @Override
  public void init( String themeName, String layers, String styles, String service, String localSRS )
  {
    super.init( themeName, layers, styles, service, localSRS );
  }

  /**
   * @see org.kalypso.ogc.gml.wms.provider.AbstractDeegreeImageProvider#getRemoteService(org.deegree.ogcwebservices.wms.capabilities.WMSCapabilities)
   */
  @Override
  protected RemoteWMService getRemoteService( WMSCapabilities capabilities )
  {
    return new RemoteWMService( capabilities );
  }

  /**
   * @see org.kalypso.ogc.gml.wms.provider.AbstractDeegreeImageProvider#loadImage(int, int,
   *      org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  @Override
  protected Image loadImage( int width, int height, GM_Envelope bbox ) throws CoreException
  {
    /* Check if nothing to request. */
    if( bbox == null )
      return null;

    /* If the some of the parameters has the wrong values, return null. */
    if( width == 0 || height == 0 || bbox.getWidth() == 0 || bbox.getHeight() == 0 )
      return null;

    /* Work locally against a copy of the reference, because it may change any time... */
    RemoteWMService remoteWMS = getWms();
    if( remoteWMS == null )
      return null;

    try
    {
      /* Create the GetMap request. */
      GetMap request = DeegreeWMSUtilities.createGetMapRequest( (WMSCapabilities) remoteWMS.getCapabilities(), getNegotiatedSRS(), getThemeName(), getLayers(), getStyles(), width, height, bbox, getLocalSRS() );

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

        MultiStatus status = new MultiStatus( KalypsoCorePlugin.getID(), 0, Messages.getString("org.kalypso.ogc.gml.wms.loader.images.WMSImageProvider.1"), null ); //$NON-NLS-1$
        status.add( StatusUtilities.createErrorStatus( Messages.getString("org.kalypso.ogc.gml.wms.loader.images.WMSImageProvider.2") + mapRequest + "'" ) ); //$NON-NLS-1$ //$NON-NLS-2$
        status.add( StatusUtilities.createErrorStatus( Messages.getString("org.kalypso.ogc.gml.wms.loader.images.WMSImageProvider.4") ) ); //$NON-NLS-1$
        status.add( StatusUtilities.createMultiStatusFromMessage( IStatus.ERROR, KalypsoCorePlugin.getID(), 0, exception.toString(), "\n", null ) ); //$NON-NLS-1$

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
   * @see org.kalypso.ogc.gml.wms.provider.AbstractDeegreeImageProvider#loadLegendGraphic(java.lang.String)
   */
  @Override
  protected org.eclipse.swt.graphics.Image loadLegendGraphic( String layerName ) throws CoreException
  {
    /* We need a remote WMS. */
    if( getWms() == null )
      return null;

    /* Check, if there is a legend graphic available. */
    WMSCapabilities capabilities = (WMSCapabilities) getWms().getCapabilities();

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

    try
    {
      /* The result image. */
      org.eclipse.swt.graphics.Image result = new org.eclipse.swt.graphics.Image( Display.getCurrent(), onlineResource.openStream() );

      return result;
    }
    catch( IOException e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.themes.provider.IKalypsoImageProvider#getLabel()
   */
  public String getLabel( )
  {
    return Messages.getString("org.kalypso.ogc.gml.wms.loader.images.WMSImageProvider.6") + getService(); //$NON-NLS-1$
  }
}