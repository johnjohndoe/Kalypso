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
package org.kalypso.model.wspm.tuhh.ui.chart.themes;

import org.eclipse.jface.resource.ImageDescriptor;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.tuhh.ui.chart.layers.ImageLegendEntry;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.view.chart.ComponentLayer;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;

import de.openali.odysseus.chart.framework.model.layer.ILegendEntry;

/**
 * @author kimwerner
 */
public class GeoCoordinateTheme extends AbstractPlaceholderProfileTheme
{
  public static final String TITLE = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.GeoCoordinateTheme.0" ); //$NON-NLS-1$

  private static final String[] COMPONENTS = new String[] { IWspmConstants.POINT_PROPERTY_HOCHWERT, IWspmConstants.POINT_PROPERTY_RECHTSWERT };

  public GeoCoordinateTheme( final IProfile profil )
  {
    super( profil, IWspmConstants.LAYER_GEOKOORDINATEN, TITLE, subLayers( profil ), COMPONENTS );
  }

  private static IProfilChartLayer[] subLayers( final IProfile profil )
  {
    return new IProfilChartLayer[] { new ComponentLayer( profil, IWspmConstants.POINT_PROPERTY_HOCHWERT ), new ComponentLayer( profil, IWspmConstants.POINT_PROPERTY_RECHTSWERT ) };
  }

  @Override
  public synchronized ILegendEntry[] getLegendEntries( )
  {
    final ImageDescriptor coordinatesImage = KalypsoGisPlugin.getImageProvider().getImageDescriptor( ImageProvider.DESCRIPTORS.STATUS_LINE_SHOW_MAP_COORDS );

    final ImageLegendEntry entry = new ImageLegendEntry( this, TITLE, coordinatesImage );

    return new ILegendEntry[] { entry };
  }

}