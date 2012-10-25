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
package org.kalypso.model.wspm.tuhh.ui.internal.chart.waterlevel;

import java.util.ArrayList;
import java.util.Collection;

import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.view.ILayerStyleProvider;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;
import org.kalypso.model.wspm.ui.view.chart.layer.wsp.IWspLayerData;
import org.kalypso.model.wspm.ui.view.chart.layer.wsp.WspLayer;

import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.model.layer.ILayerManager;
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;

/**
 * @author Dirk Kuch
 */
public class WspFixationLayer extends WspLayer
{
  public WspFixationLayer( final IProfile profile, final String layerId, final ILayerStyleProvider styleProvider, final IWspLayerData data, final ICoordinateMapper< ? , ? > mapper )
  {
    super( profile, layerId, new IProfilChartLayer[] { new WspPointsLayer( layerId, profile, data ), new WspSegmentsLayer( layerId, profile, data ) }, styleProvider, data, mapper, new WaterLevelFixationFilter() );

    setTitle( Messages.getString( "WspFixationLayer_0" ) ); //$NON-NLS-1$
  }

  @Override
  public IChartLayer[] getLegendNodes( )
  {
    final Collection<IChartLayer> layers = new ArrayList<>( 2 );

    final ILayerManager layerManager = getLayerManager();
    final IChartLayer[] children = layerManager.getLayers();
    for( final IChartLayer child : children )
    {
      if( child instanceof WspObjectsLayer )
      {
        final WspObjectsLayer wspLayer = (WspObjectsLayer)child;
        if( wspLayer.hasSomethingToShow() )
          layers.add( wspLayer );
      }
    }

    return layers.toArray( new IChartLayer[layers.size()] );
  }
}