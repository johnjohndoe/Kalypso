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
package org.kalypso.model.wspm.tuhh.ui.chart;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.kalypso.model.wspm.core.IWspmLayers;
import org.kalypso.model.wspm.core.IWspmPhenomenonConstants;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode;
import org.kalypso.model.wspm.tuhh.ui.chart.layers.PointMarkerLayer;
import org.kalypso.model.wspm.tuhh.ui.chart.layers.RiverChannelLayer;
import org.kalypso.model.wspm.tuhh.ui.chart.layers.RoughnessLayer;
import org.kalypso.model.wspm.tuhh.ui.chart.themes.BuildingBridgeTheme;
import org.kalypso.model.wspm.tuhh.ui.chart.themes.BuildingWeirTheme;
import org.kalypso.model.wspm.tuhh.ui.chart.themes.DeviderTheme;
import org.kalypso.model.wspm.tuhh.ui.chart.themes.RoughnessTheme;
import org.kalypso.model.wspm.tuhh.ui.chart.themes.VegetationTheme;
import org.kalypso.model.wspm.tuhh.ui.internal.chart.waterlevel.TuhhResultDataProvider;
import org.kalypso.model.wspm.tuhh.ui.internal.chart.waterlevel.WaterLevelFilter;
import org.kalypso.model.wspm.tuhh.ui.internal.chart.waterlevel.WspFixationLayer;
import org.kalypso.model.wspm.ui.view.ILayerStyleProvider;
import org.kalypso.model.wspm.ui.view.chart.ComponentLayer;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;
import org.kalypso.model.wspm.ui.view.chart.PointsLineLayer;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartModel;
import org.kalypso.model.wspm.ui.view.chart.layer.CrossSectionTheme;
import org.kalypso.model.wspm.ui.view.chart.layer.StationLineLayer;
import org.kalypso.model.wspm.ui.view.chart.layer.wsp.IWspLayerData;
import org.kalypso.model.wspm.ui.view.chart.layer.wsp.WspLayer;
import org.kalypso.observation.phenomenon.IPhenomenon;
import org.kalypso.observation.result.IComponent;

import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.model.layer.ILayerManager;
import de.openali.odysseus.chart.framework.model.mapper.IAxis;
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;
import de.openali.odysseus.chart.framework.model.mapper.impl.CoordinateMapper;

/**
 * @author Dirk Kuch
 */
final class TuhhLayers
{
  private TuhhLayers( )
  {
  }

  public static IProfilChartLayer createBridgeLayer( final IProfile profil, final ICoordinateMapper mapper, final LayerStyleProviderTuhh styleProvider )
  {
    final PointsLineLayer unterkante = new PointsLineLayer( IWspmTuhhConstants.LAYER_BRUECKE + "_" + IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, profil, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, styleProvider ); //$NON-NLS-1$
    final PointsLineLayer oberkante = new PointsLineLayer( IWspmTuhhConstants.LAYER_BRUECKE + "_" + IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, profil, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, styleProvider ); //$NON-NLS-1$

    return new BuildingBridgeTheme( profil, new IProfilChartLayer[] { unterkante, oberkante }, mapper );
  }

  public static IProfilChartLayer createWehrLayer( final IProfile profil, final ICoordinateMapper axisMapper, final ICoordinateMapper screenMapper, final LayerStyleProviderTuhh styleProvider )
  {
    final PointsLineLayer oberkante = new PointsLineLayer( IWspmTuhhConstants.LAYER_WEHR + "_" + IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, profil, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, styleProvider ); //$NON-NLS-1$
    final PointMarkerLayer pointMarkerLayer = new PointMarkerLayer( profil, IWspmTuhhConstants.MARKER_TYP_WEHR, styleProvider, 30, false );

    return new BuildingWeirTheme( profil, new IProfilChartLayer[] { oberkante, pointMarkerLayer }, axisMapper, screenMapper );
  }

  public static IProfilChartLayer createDeviderLayer( final IProfile profil, final ICoordinateMapper cmScreen, final LayerStyleProviderTuhh styleProvider )
  {
    final PointMarkerLayer dbLayer = new PointMarkerLayer( profil, IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, styleProvider, 5, true );
    final RiverChannelLayer tfLayer = new RiverChannelLayer( profil, styleProvider, 15, false );
    final PointMarkerLayer bvLayer = new PointMarkerLayer( profil, IWspmTuhhConstants.MARKER_TYP_BORDVOLL, styleProvider, 25, false );
    final IProfilChartLayer[] subLayers = new IProfilChartLayer[] { bvLayer, tfLayer, dbLayer };

    return new DeviderTheme( profil, subLayers, cmScreen );
  }

  public static IProfilChartLayer createRoughnessLayer( final IProfile profil, final ICoordinateMapper mapper, final LayerStyleProviderTuhh styleProvider )
  {
    final RoughnessLayer ks = new RoughnessLayer( IWspmTuhhConstants.LAYER_RAUHEIT_KS, profil, IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS, styleProvider ); //$NON-NLS-1$
    final RoughnessLayer kst = new RoughnessLayer( IWspmTuhhConstants.LAYER_RAUHEIT_KST, profil, IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST, styleProvider ); //$NON-NLS-1$
    // final RoughnessLayer clazz = new RoughnessLayer( profil, IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_CLASS,
    // styleProvider ); //TODO see todo in RoughnessLayser.getValue()

    final IProfilChartLayer[] subLayers = new IProfilChartLayer[] { kst, ks };

    return new RoughnessTheme( profil, subLayers, mapper );
  }

  public static IProfilChartLayer createGelaendeLayer( final IProfile profil, final ICoordinateMapper mapper, final LayerStyleProviderTuhh styleProvider )
  {
    final PointsLineLayer stationPointLayer = new PointsLineLayer( IWspmLayers.LAYER_GELAENDE, profil, IWspmPointProperties.POINT_PROPERTY_HOEHE, styleProvider ); //$NON-NLS-1$
    final StationLineLayer stationLineLayer = new StationLineLayer( IWspmLayers.LAYER_STATION_LINES, profil, IWspmPointProperties.POINT_PROPERTY_HOEHE, styleProvider );

    return new CrossSectionTheme( profil, new IProfilChartLayer[] { stationLineLayer, stationPointLayer }, mapper );
  }

  public static IProfilChartLayer createVegetationLayer( final IProfile profil, final ICoordinateMapper cmLeft, final LayerStyleProviderTuhh styleProvider )
  {
    final ComponentLayer ax = new ComponentLayer( profil, IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AX, false );
    final ComponentLayer ay = new ComponentLayer( profil, IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AY, false );
    final ComponentLayer dp = new ComponentLayer( profil, IWspmPointProperties.POINT_PROPERTY_BEWUCHS_DP, false );

    return new VegetationTheme( profil, new IProfilChartLayer[] { ax, ay, dp }, cmLeft, styleProvider );
  }

  public static String[] getExistingLayers( final ProfilChartModel model )
  {
    final List<String> existing = new ArrayList<>();

    final ILayerManager manager = model.getLayerManager();
    final IChartLayer[] layers = manager.getLayers();

    for( final IChartLayer layer : layers )
    {
      existing.add( layer.getIdentifier() );
    }

    return existing.toArray( new String[] {} );
  }

  public static IProfilChartLayer[] create2DWaterLevelLayers( final IProfile profile, final IAxis domainAxis, final IAxis targetAxis, final ILayerStyleProvider styleProvider )
  {
    final Set<IProfilChartLayer> layers = new LinkedHashSet<>();

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

  public static IProfilChartLayer createWspLayer( final IProfile profile, final IWspmResultNode result, final ICoordinateMapper cm, final ILayerStyleProvider styleProvider )
  {
    final TuhhResultDataProvider wspLayerData = new TuhhResultDataProvider( profile, result, "activeIds" ); //$NON-NLS-1$

    final WaterLevelFilter wspFilter = new WaterLevelFilter();
    final boolean hasWspData = wspLayerData.hasWspData( wspFilter );
    if( !hasWspData )
      return null;

    return new WspLayer( profile, IWspmLayers.LAYER_WASSERSPIEGEL, new IProfilChartLayer[] {}, styleProvider, wspLayerData, cm, wspFilter );
  }

  public static IProfilChartLayer createWspFixationLayer( final IProfile profile, final IWspmResultNode result, final ICoordinateMapper cm, final ILayerStyleProvider styleProvider )
  {
    final IWspLayerData data = new TuhhResultDataProvider( profile, result, "activeFixationIds" ); //$NON-NLS-1$

    return new WspFixationLayer( profile, IWspmLayers.LAYER_WASSERSPIEGEL_FIXIERUNG, styleProvider, data, cm );
  }
}