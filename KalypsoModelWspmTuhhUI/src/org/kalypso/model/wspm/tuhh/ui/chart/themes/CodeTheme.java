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
import org.kalypso.model.wspm.core.IWspmLayers;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIImages;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.chart.layers.ImageLegendEntry;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;

import de.openali.odysseus.chart.framework.model.layer.ILegendEntry;

/**
 * @author Dirk Kuch
 */
public class CodeTheme extends AbstractPlaceholderProfileTheme
{
  public static final String TITLE = "Code"; //$NON-NLS-1$

  public CodeTheme( final IProfile profil )
  {
    super( profil, IWspmLayers.LAYER_CODE, TITLE, new IProfilChartLayer[] {}, new String[] { IWspmPointProperties.POINT_PROPERTY_CODE } );
  }

  @Override
  public synchronized ILegendEntry[] getLegendEntries( )
  {
    final ImageDescriptor sinusImage = KalypsoModelWspmTuhhUIPlugin.getImageProvider().getImageDescriptor( KalypsoModelWspmTuhhUIImages.LAYER_CODE );

    final ImageLegendEntry entry = new ImageLegendEntry( this, getTitle(), sinusImage );

    return new ILegendEntry[] { entry };
  }
}
