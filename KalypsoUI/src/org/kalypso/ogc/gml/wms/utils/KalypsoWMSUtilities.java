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
package org.kalypso.ogc.gml.wms.utils;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Properties;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.kalypso.commons.java.util.PropertiesHelper;
import org.kalypso.ogc.gml.wms.provider.IKalypsoImageProvider;
import org.kalypso.ogc.gml.wms.provider.KalypsoWMSImageProvider;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * This is a helper class, providing functions for dealing with a WMS.
 * 
 * @author Holger Albert
 */
public class KalypsoWMSUtilities
{
  /**
   * The constructor.
   */
  private KalypsoWMSUtilities( )
  {
  }

  /**
   * This function should return an image provider vor the given source. It chooses with the PROVIDER attribute and
   * returns the specific provider. If none could be found using that source of information, an default one ({@link org.kalypso.ogc.gml.map.themes.provider.KalypsoWMSImageProvider}
   * will be returned.
   * 
   * @param themeName
   *            The name of the theme.
   * @param source
   *            Some parameters for the WMS.
   * @param localSRS
   *            The client coordinate system.
   * @return An image provider. Should never be null.
   */
  public static IKalypsoImageProvider getImageProvider( String themeName, String source, CS_CoordinateSystem localSRS )
  {
    /* Parse the source into properties. */
    Properties sourceProps = PropertiesHelper.parseFromString( source, '#' );

    /* Get the provider attribute. */
    String providerID = sourceProps.getProperty( IKalypsoImageProvider.KEY_PROVIDER, null );

    /* If it is missing or null, return the default provider. */
    if( providerID == null )
      return getDefaultImageProvider( themeName, source, localSRS );

    /* Get the extension registry. */
    IExtensionRegistry er = Platform.getExtensionRegistry();

    /* The registry must exist. */
    if( er == null )
      return getDefaultImageProvider( themeName, source, localSRS );

    IConfigurationElement[] configurationElementsFor = er.getConfigurationElementsFor( "org.kalypso.ui.addlayer.WMSImageProvider" );
    for( IConfigurationElement element : configurationElementsFor )
    {
      /* Get some attributes. */
      String id = element.getAttribute( "id" );
      if( id.equals( providerID ) )
      {
        try
        {
          /* This is the wanted provider. */
          IKalypsoImageProvider provider = (IKalypsoImageProvider) element.createExecutableExtension( "class" );
          provider.init( themeName, source, localSRS );

          return provider;
        }
        catch( CoreException e )
        {
          e.printStackTrace();

          return getDefaultImageProvider( themeName, source, localSRS );
        }
      }
    }

    return getDefaultImageProvider( themeName, source, localSRS );
  }

  /**
   * This function returns the default wms image provider. It initializes it, too.
   * 
   * @param themeName
   *            The name of the theme.
   * @param source
   *            Some parameters for the WMS.
   * @param localSRS
   *            The client coordinate system.
   * @return The default wms image provider.
   */
  public static IKalypsoImageProvider getDefaultImageProvider( String themeName, String source, CS_CoordinateSystem localSRS )
  {
    KalypsoWMSImageProvider provider = new KalypsoWMSImageProvider();
    provider.init( themeName, source, localSRS );

    return provider;
  }

  /**
   * This function creates an URL for a get capabilities request from the given base URL.
   * 
   * @param baseURL
   *            The base URL.
   * @return The get capabilities request URL.
   */
  public static URL createCapabilitiesRequest( URL baseURL ) throws MalformedURLException
  {
    String query = baseURL.getQuery();
    String getCapabilitiesQuery = "SERVICE=WMS&VERSION=1.1.1&REQUEST=GetCapabilities";
    String queryToken = (query == null || query.length() == 0) ? "?" : "&";
    String urlGetCapabilitiesString = baseURL.toString() + queryToken + getCapabilitiesQuery;

    return new URL( urlGetCapabilitiesString );
  }
}