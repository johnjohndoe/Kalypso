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
package org.kalypso.model.wspm.tuhh.ui.chart.utils;

import java.util.LinkedHashSet;
import java.util.Set;

import org.kalypso.commons.java.lang.Arrays;
import org.kalypso.model.wspm.core.IWspmLayers;
import org.kalypso.model.wspm.core.IWspmPhenomenonConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.Buildings;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.IProfileBuilding;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode;
import org.kalypso.model.wspm.tuhh.ui.chart.data.TuhhResultDataProvider;
import org.kalypso.model.wspm.ui.view.ILayerStyleProvider;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;
import org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider;
import org.kalypso.model.wspm.ui.view.chart.PointsLineLayer;
import org.kalypso.model.wspm.ui.view.chart.layer.wsp.IWspLayerData;
import org.kalypso.model.wspm.ui.view.chart.layer.wsp.WspLayer;
import org.kalypso.model.wspm.ui.view.chart.layer.wspfixation.WspFixationLayer;
import org.kalypso.observation.phenomenon.IPhenomenon;
import org.kalypso.observation.result.IComponent;

import de.openali.odysseus.chart.framework.model.mapper.IAxis;
import de.openali.odysseus.chart.framework.model.mapper.impl.CoordinateMapper;

/**
 * @author Dirk Kuch
 */
public final class TuhhLayerCreator
{
  private TuhhLayerCreator( )
  {
  }

  public static IProfilChartLayer[] create2DWaterLevelLayers( final IProfil profile, final IAxis domainAxis, final IAxis targetAxis, final ILayerStyleProvider styleProvider )
  {
    final Set<IProfilChartLayer> layers = new LinkedHashSet<IProfilChartLayer>();

    final IComponent[] pointProperties = profile.getPointProperties();
    for( final IComponent property : pointProperties )
    {
      final IPhenomenon phenomenon = property.getPhenomenon();

      if( IWspmPhenomenonConstants.PHENOMENON_WATERLEVEL_2D.equals( phenomenon.getID() ) )
      {
        final PointsLineLayer layer = new PointsLineLayer( IWspmLayers.LAYER_WASSERSPIEGEL2D, profile, property.getId(), styleProvider );
        layer.setTitle( property.getName() );

        layer.setCoordinateMapper( new CoordinateMapper( domainAxis, targetAxis ) );
        layers.add( layer );
      }
    }

    return layers.toArray( new IProfilChartLayer[] {} );
  }

  public static IProfilChartLayer createBuildingLayer( final IProfil profile, final IProfilLayerProvider provider )
  {
    // TODO IProfileObjects now returned as list from IProfile, but we can only handle one IProfileObject (WSPM can't
    // handle more!)
    final IProfileObject[] buildings = profile.getProfileObjects( IProfileBuilding.class );
    if( Arrays.isEmpty( buildings ) )
      return null;

    for( final IProfileObject building : buildings )
    {
      if( Buildings.isBridge( building ) )
        return provider.createLayer( profile, IWspmTuhhConstants.LAYER_BRUECKE );
      else if( Buildings.isWeir( building ) )
        return provider.createLayer( profile, IWspmTuhhConstants.LAYER_WEHR );
      else if( Buildings.isTube( building ) )
        return provider.createLayer( profile, IWspmTuhhConstants.LAYER_TUBES );
      else if( building.getId().equals( IWspmTuhhConstants.BUILDING_TYP_KREIS ) )
        return provider.createLayer( profile, IWspmTuhhConstants.LAYER_TUBES );
      else if( building.getId().equals( IWspmTuhhConstants.BUILDING_TYP_EI ) )
        return provider.createLayer( profile, IWspmTuhhConstants.LAYER_TUBES );
    }

    return null;
  }

  public static IProfilChartLayer createWspLayer( final IProfil profile, final IWspmResultNode result, final IAxis domainAxis, final IAxis targetAxis, final ILayerStyleProvider styleProvider )
  {

    final CoordinateMapper cm = new CoordinateMapper( domainAxis, targetAxis );
    final IWspLayerData wspLayerData = new TuhhResultDataProvider( result, "activeIds" ); //$NON-NLS-1$

    return new WspLayer( profile, IWspmLayers.LAYER_WASSERSPIEGEL, styleProvider, wspLayerData, false, cm );
  }

  public static IProfilChartLayer createWspFixationLayer( final IProfil profile, final IWspmResultNode result, final IAxis domainAxis, final IAxis targetAxis, final ILayerStyleProvider styleProvider )
  {
    final CoordinateMapper cm = new CoordinateMapper( domainAxis, targetAxis );
    final IWspLayerData data = new TuhhResultDataProvider( result, "activeFixationIds" ); //$NON-NLS-1$

    return new WspFixationLayer( profile, IWspmLayers.LAYER_WASSERSPIEGEL_FIXIERUNG, styleProvider, data, false, cm );
  }
}
