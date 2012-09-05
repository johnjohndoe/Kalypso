/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit.overlay;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;

import de.openali.odysseus.chart.framework.model.mapper.impl.CoordinateMapper;

/**
 * @author kimwerner
 */
public class ProfilOverlayLayerProvider extends ProfilLayerProviderTuhh
{
  @Override
  public IProfilChartLayer[] createLayers( final IProfil profil, final Object result )
  {
    final List<IProfilChartLayer> layers = new ArrayList<>();

    final ProfilOverlayLayer overlay = new ProfilOverlayLayer( profil, getLsp() );
    overlay.setCoordinateMapper( new CoordinateMapper( getDomainAxis(), getTargetAxisLeft() ) );
    layers.add( overlay );

    final IProfilChartLayer[] superLayers = super.createLayers( profil, result );
    for( final IProfilChartLayer layer : superLayers )
    {
      /* Suppress editing for all layers excpet my own */
      layer.lockLayer( true );
      layers.add( layer );
    }

    return layers.toArray( new IProfilChartLayer[layers.size()] );
  }
}
