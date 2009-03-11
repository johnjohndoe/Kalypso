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
package org.kalypso.ogc.gml.map.themes;

import java.net.URL;
import java.util.Properties;

import org.kalypso.commons.i18n.I10nString;
import org.kalypso.commons.java.util.PropertiesHelper;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.IKalypsoThemeFactory;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.wms.provider.images.IKalypsoImageProvider;
import org.kalypso.ogc.gml.wms.utils.KalypsoWMSUtilities;
import org.kalypso.template.types.StyledLayerType;

/**
 * Theme factory for {@link KalypsoWMSTheme}s.
 * 
 * @author Gernot Belger
 */
public class WmsThemeFactory implements IKalypsoThemeFactory
{
  /**
   * @see org.kalypso.ogc.gml.IKalypsoThemeFactory#createTheme(org.kalypso.commons.i18n.I10nString,
   *      org.kalypso.template.types.StyledLayerType, java.net.URL, org.kalypso.ogc.gml.mapmodel.IMapModell,
   *      org.kalypso.ogc.gml.selection.IFeatureSelectionManager)
   */
  @Override
  public IKalypsoTheme createTheme( final I10nString layerName, final StyledLayerType layerType, final URL context, final IMapModell mapModell, final IFeatureSelectionManager selectionManager ) 
  {
    final String source = layerType.getHref();
    final String linktype = layerType.getLinktype();

    /* Parse the source into properties. */
    final Properties sourceProps = PropertiesHelper.parseFromString( source, '#' );

    /* Get the provider attribute. */
    final String layerProp = sourceProps.getProperty( IKalypsoImageProvider.KEY_LAYERS, null );
    final String styleProp = sourceProps.getProperty( IKalypsoImageProvider.KEY_STYLES, null );
    final String service = sourceProps.getProperty( IKalypsoImageProvider.KEY_URL, null );
    final String providerID = sourceProps.getProperty( IKalypsoImageProvider.KEY_PROVIDER, null );

    /* Create the image provider. */
    final String[] layers = layerProp == null ? null : layerProp.split( "," );
    final String[] styles = styleProp == null ? null : styleProp.split( "," );
    final IKalypsoImageProvider imageProvider = KalypsoWMSUtilities.getImageProvider( layerName.getValue(), layers, styles, service, providerID );

    return new KalypsoWMSTheme( source, linktype, layerName, imageProvider, mapModell );
  }

}
