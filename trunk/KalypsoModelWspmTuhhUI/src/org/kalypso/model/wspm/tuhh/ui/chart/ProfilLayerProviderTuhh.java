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
package org.kalypso.model.wspm.tuhh.ui.chart;

import java.awt.Insets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.IWspmLayers;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfilePointPropertyProvider;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.energyloss.IEnergylossProfileObject;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.IProfileBuilding;
import org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.ISinuositaetProfileObject;
import org.kalypso.model.wspm.tuhh.core.profile.utils.TuhhProfiles;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode;
import org.kalypso.model.wspm.tuhh.ui.chart.layers.EnergylossLayer;
import org.kalypso.model.wspm.tuhh.ui.chart.layers.SinuositaetLayer;
import org.kalypso.model.wspm.tuhh.ui.chart.themes.BuildingBridgeTheme;
import org.kalypso.model.wspm.tuhh.ui.chart.themes.BuildingTubesTheme;
import org.kalypso.model.wspm.tuhh.ui.chart.themes.BuildingWeirTheme;
import org.kalypso.model.wspm.tuhh.ui.chart.themes.CodeTheme;
import org.kalypso.model.wspm.tuhh.ui.chart.themes.CommentTheme;
import org.kalypso.model.wspm.tuhh.ui.chart.themes.DeviderTheme;
import org.kalypso.model.wspm.tuhh.ui.chart.themes.GeoCoordinateTheme;
import org.kalypso.model.wspm.tuhh.ui.chart.themes.RoughnessTheme;
import org.kalypso.model.wspm.tuhh.ui.chart.themes.VegetationTheme;
import org.kalypso.model.wspm.tuhh.ui.chart.utils.LayerStyleProviderTuhh;
import org.kalypso.model.wspm.tuhh.ui.chart.utils.TuhhLayerCreator;
import org.kalypso.model.wspm.tuhh.ui.chart.utils.TuhhLayers;
import org.kalypso.model.wspm.tuhh.ui.chart.utils.TuhhLayersAdder;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;
import org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider;
import org.kalypso.model.wspm.ui.view.chart.LayerDescriptor;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartModel;
import org.kalypso.model.wspm.ui.view.chart.layer.CrossSectionTheme;
import org.kalypso.model.wspm.ui.view.table.handler.WspmTableUiHandlerProvider;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandlerProvider;

import de.openali.odysseus.chart.ext.base.axis.GenericLinearAxis;
import de.openali.odysseus.chart.ext.base.axis.ScreenCoordinateAxis;
import de.openali.odysseus.chart.ext.base.axisrenderer.AxisRendererConfig;
import de.openali.odysseus.chart.framework.model.mapper.IAxis;
import de.openali.odysseus.chart.framework.model.mapper.IAxisConstants.POSITION;
import de.openali.odysseus.chart.framework.model.mapper.impl.AxisAdjustment;
import de.openali.odysseus.chart.framework.model.mapper.impl.CoordinateMapper;
import de.openali.odysseus.chart.framework.model.mapper.registry.IMapperRegistry;
import de.openali.odysseus.chart.framework.model.style.IStyleConstants.LINECAP;
import de.openali.odysseus.chart.framework.model.style.IStyleConstants.LINEJOIN;
import de.openali.odysseus.chart.framework.util.StyleUtils;
import de.openali.odysseus.chart.framework.util.img.ChartLabelRendererFactory;

/**
 * @author kimwerner
 */
public class ProfilLayerProviderTuhh implements IProfilLayerProvider, IWspmTuhhConstants
{
  private final LayerStyleProviderTuhh m_styleProvider = new LayerStyleProviderTuhh();

  private final IAxis m_domainAxis;

  private final IAxis m_targetAxisLeft;

  private final IAxis m_targetAxisRight;

  private final IAxis m_screenAxisVertical;

  private static final String AXIS_LABEL = "[%s]"; //$NON-NLS-1$

  public ProfilLayerProviderTuhh( )
  {
    m_styleProvider.createStyles();

    final AxisRendererConfig axisRendererConfigLR = new AxisRendererConfig();
    axisRendererConfigLR.axisLineStyle.setLineCap( LINECAP.FLAT );
    axisRendererConfigLR.tickLineStyle.setLineCap( LINECAP.FLAT );
    axisRendererConfigLR.axisLineStyle.setLineJoin( LINEJOIN.BEVEL );
    axisRendererConfigLR.tickLineStyle.setLineJoin( LINEJOIN.BEVEL );
    axisRendererConfigLR.axisLineStyle.setWidth( 2 );
    axisRendererConfigLR.tickLineStyle.setWidth( 2 );
    axisRendererConfigLR.axisInsets = new Insets( 0, 0, 0, 0 );
    axisRendererConfigLR.hideCut = false;
    final AxisRendererConfig axisRendererConfigD = new AxisRendererConfig();
    axisRendererConfigD.axisLineStyle.setLineCap( LINECAP.FLAT );
    axisRendererConfigD.tickLineStyle.setLineCap( LINECAP.FLAT );
    axisRendererConfigD.axisLineStyle.setLineJoin( LINEJOIN.BEVEL );
    axisRendererConfigD.tickLineStyle.setLineJoin( LINEJOIN.BEVEL );
    axisRendererConfigD.axisLineStyle.setDash( 0, null );
    axisRendererConfigD.tickLineStyle.setDash( 0, null );
    axisRendererConfigD.axisLineStyle.setWidth( 2 );
    axisRendererConfigD.tickLineStyle.setWidth( 2 );
    axisRendererConfigD.axisInsets = new Insets( 0, 0, 0, 0 );
    axisRendererConfigD.hideCut = true;

    m_screenAxisVertical = new ScreenCoordinateAxis( "ProfilLayerProviderTuhh_AXIS_VERTICAL_SCREEN", POSITION.RIGHT );//$NON-NLS-1$
    m_screenAxisVertical.setPreferredAdjustment( new AxisAdjustment( 0, 1, 0 ) );
    m_domainAxis = new GenericLinearAxis( "ProfilLayerProviderTuhh_AXIS_DOMAIN", POSITION.BOTTOM, axisRendererConfigD );//$NON-NLS-1$
    m_domainAxis.setPreferredAdjustment( new AxisAdjustment( 3, 94, 3 ) );
    m_targetAxisLeft = new GenericLinearAxis( "ProfilLayerProviderTuhh_AXIS_LEFT", POSITION.LEFT, axisRendererConfigLR );//$NON-NLS-1$
    m_targetAxisLeft.setPreferredAdjustment( new AxisAdjustment( 15, 75, 10 ) );
    m_targetAxisRight = new GenericLinearAxis( "ProfilLayerProviderTuhh_AXIS_RIGHT", POSITION.RIGHT, axisRendererConfigLR );//$NON-NLS-1$
    m_targetAxisRight.setPreferredAdjustment( new AxisAdjustment( 2, 40, 58 ) );
  }

  // FIXME: once, this was nice and object oriented; we should do it agin like this: encapsulate adding layer into a class! -> this class is presented in the ui
  @Override
  public void addLayerToProfile( final Shell shell, final IProfile profil, final String layerId )
  {
    final IProfilePointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profil.getType() );

    if( layerId.equals( IWspmTuhhConstants.LAYER_BEWUCHS ) )
    {
      TuhhLayersAdder.addVegetationLayer( provider, profil );
    }
    else if( layerId.equals( IWspmLayers.LAYER_GEOKOORDINATEN ) )
    {
      TuhhLayersAdder.addGeoLayer( provider, profil );
    }
    else if( layerId.equals( IWspmLayers.LAYER_GELAENDE ) )
    {
      TuhhLayersAdder.addGroundLayer( provider, profil );
    }
    else if( layerId.equals( IWspmTuhhConstants.LAYER_RAUHEIT ) )
    {
      TuhhLayersAdder.addRoughnessLayers( provider, profil, m_targetAxisRight, AXIS_LABEL );
    }
    else if( layerId.equals( IWspmTuhhConstants.LAYER_BRUECKE ) )
    {
      TuhhLayersAdder.addBridgeLayer( shell, profil );
    }
    else if( layerId.equals( IWspmTuhhConstants.LAYER_WEHR ) )
    {
      TuhhLayersAdder.addWeirLayer( shell, profil );
    }
    else if( layerId.equals( IWspmTuhhConstants.LAYER_TUBES ) )
    {
      TuhhLayersAdder.addTubesLayer( shell, profil );
    }
    else if( layerId.equals( IWspmTuhhConstants.LAYER_SINUOSITAET ) )
    {
      TuhhLayersAdder.addSinuositaetLayer( profil );
    }
    else if( layerId.equals( IWspmTuhhConstants.LAYER_ENERGYLOSS ) )
    {
      TuhhLayersAdder.addEnergylossLayer( profil );
    }
    if( layerId.equals( IWspmTuhhConstants.LAYER_COMMENT ) )
    {
      TuhhLayersAdder.addCommentLayer( provider, profil );
    }
    else if( layerId.equals( IWspmLayers.LAYER_CODE ) )
    {
      TuhhLayersAdder.addCodeLayer( provider, profil );
    }
  }

  @Override
  public IProfilChartLayer createLayer( final IProfile profil, final String layerID )
  {
    if( layerID == null || profil == null )
      return null;

    final CoordinateMapper cmLeft = new CoordinateMapper( m_domainAxis, m_targetAxisLeft );
    final CoordinateMapper cmRight = new CoordinateMapper( m_domainAxis, m_targetAxisRight );
    final CoordinateMapper cmScreen = new CoordinateMapper( m_domainAxis, m_screenAxisVertical );

    switch( layerID )
    {
      case IWspmTuhhConstants.LAYER_BEWUCHS:
        return TuhhLayers.createVegetationLayer( profil, cmLeft, m_styleProvider );

      case IWspmConstants.LAYER_GEOKOORDINATEN:
        return new GeoCoordinateTheme( profil );

      case IWspmConstants.LAYER_COMMENT:
        return new CommentTheme( profil );

      case IWspmConstants.LAYER_CODE:
        return new CodeTheme( profil );

      case IWspmLayers.LAYER_GELAENDE:
        return TuhhLayers.createGelaendeLayer( profil, cmLeft, m_styleProvider );

      case IWspmTuhhConstants.LAYER_RAUHEIT:
        return TuhhLayers.createRoughnessLayer( profil, cmRight, m_styleProvider );

      case IWspmTuhhConstants.LAYER_BRUECKE:
        return TuhhLayers.createBridgetLayer( profil, cmLeft, m_styleProvider );

      case IWspmTuhhConstants.LAYER_WEHR:
        return TuhhLayers.createWehrLayer( profil, layerID, cmLeft, cmScreen, m_styleProvider );

      case IWspmTuhhConstants.LAYER_TUBES:
        return new BuildingTubesTheme( profil, cmLeft, m_styleProvider );

      case IWspmTuhhConstants.LAYER_SINUOSITAET:
        return new SinuositaetLayer( profil );

      case IWspmTuhhConstants.LAYER_DEVIDER:
        return TuhhLayers.createDeviderLayer( profil, cmScreen, m_styleProvider );

      case IWspmTuhhConstants.LAYER_ENERGYLOSS:
        return new EnergylossLayer( profil );

      default:
        return null;
    }
  }

  @Override
  public IProfilChartLayer[] createLayers( final IProfile profile, final Object result )
  {
    // Achtung: diese Reihenfolge ist die natürliche Ordnung im Layermanager
    final List<IProfilChartLayer> layersToAdd = new ArrayList<>();

    /** geo coordinates */
    if( profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_HOCHWERT ) != null )
      layersToAdd.add( createLayer( profile, IWspmLayers.LAYER_GEOKOORDINATEN ) );

    /** roughnesses */
    if( TuhhProfiles.hasRoughness( profile ) )
      layersToAdd.add( createLayer( profile, IWspmTuhhConstants.LAYER_RAUHEIT ) );

    /** vegetation layer */
    if( TuhhProfiles.hasVegetation( profile ) )
    {
      layersToAdd.add( createLayer( profile, IWspmTuhhConstants.LAYER_BEWUCHS ) );
    }

    /** profile buildings layer */
    final IProfilChartLayer buildingLayer = TuhhLayerCreator.createBuildingLayer( profile, this );
    if( Objects.isNotNull( buildingLayer ) )
    {
      layersToAdd.add( buildingLayer );
    }

    /** water level layer */
    layersToAdd.add( TuhhLayerCreator.createWspLayer( profile, (IWspmResultNode)result, m_domainAxis, m_targetAxisLeft, m_styleProvider ) );

    /** water level fixation layer */
    layersToAdd.add( TuhhLayerCreator.createWspFixationLayer( profile, (IWspmResultNode)result, m_domainAxis, m_targetAxisLeft, m_styleProvider ) );

    /** 2d layers */
    final IProfilChartLayer[] twoDLayers = TuhhLayerCreator.create2DWaterLevelLayers( profile, m_domainAxis, m_targetAxisLeft, m_styleProvider );
    Collections.addAll( layersToAdd, twoDLayers );

    /** ground layer */
    if( profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_HOEHE ) != null )
    {
      layersToAdd.add( createLayer( profile, IWspmLayers.LAYER_GELAENDE ) );
    }

    /** profile deviders (trennflächen) */
    layersToAdd.add( createLayer( profile, IWspmTuhhConstants.LAYER_DEVIDER ) );

    /** sinuosität profile layer */
    final ISinuositaetProfileObject[] sinObj = profile.getProfileObjects( ISinuositaetProfileObject.class );
    if( ArrayUtils.isNotEmpty( sinObj ) )
    {
      layersToAdd.add( createLayer( profile, IWspmTuhhConstants.LAYER_SINUOSITAET ) );
    }
    /** energyloss profile layer */
    final IEnergylossProfileObject[] elpo = profile.getProfileObjects( IEnergylossProfileObject.class );
    if( ArrayUtils.isNotEmpty( elpo ) )
    {
      layersToAdd.add( createLayer( profile, IWspmTuhhConstants.LAYER_ENERGYLOSS ) );
    }
    if( profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_COMMENT ) != null )
      layersToAdd.add( createLayer( profile, IWspmConstants.LAYER_COMMENT ) );
    if( profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_CODE ) != null )
      layersToAdd.add( createLayer( profile, IWspmConstants.LAYER_CODE ) );

    /* Prune 'null's returned from createLayer-subroutines */
    layersToAdd.removeAll( Collections.singleton( null ) );

    // TODO why here ?
    setAxisLabel( profile );

    return layersToAdd.toArray( new IProfilChartLayer[layersToAdd.size()] );
  }

  @Override
  public LayerDescriptor[] getAddableLayers( final ProfilChartModel chartModel )
  {

    final IProfile profile = chartModel.getProfil();
    if( Objects.isNull( profile ) )
      return new LayerDescriptor[] {};

    final List<LayerDescriptor> addable = new ArrayList<>();
    final String[] existing = TuhhLayers.getExistingLayers( chartModel );

    final IProfileObject[] objects = profile.getProfileObjects( IProfileBuilding.class );

    // only ONE Object allowed
    if( ArrayUtils.isEmpty( objects ) )
    {
      addable.add( new LayerDescriptor( BuildingBridgeTheme.TITLE, LAYER_BRUECKE ) );
      addable.add( new LayerDescriptor( BuildingWeirTheme.TITLE, IWspmTuhhConstants.LAYER_WEHR ) );
      addable.add( new LayerDescriptor( BuildingTubesTheme.TITLE, IWspmTuhhConstants.LAYER_TUBES ) );
    }
    // always show devider and roughness
    if( !ArrayUtils.contains( existing, IWspmTuhhConstants.LAYER_RAUHEIT ) )
    {
      addable.add( new LayerDescriptor( RoughnessTheme.TITLE, IWspmTuhhConstants.LAYER_RAUHEIT ) );
    }
    if( !ArrayUtils.contains( existing, IWspmTuhhConstants.LAYER_DEVIDER ) )
    {
      addable.add( new LayerDescriptor( DeviderTheme.TITLE, IWspmTuhhConstants.LAYER_DEVIDER ) );
    }

    if( Objects.isNull( profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AX ) ) && !ArrayUtils.contains( existing, IWspmTuhhConstants.LAYER_BEWUCHS ) )
    {
      addable.add( new LayerDescriptor( VegetationTheme.TITLE, IWspmTuhhConstants.LAYER_BEWUCHS ) );
    }
    if( Objects.isNull( profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_HOEHE ) ) && !ArrayUtils.contains( existing, IWspmLayers.LAYER_GELAENDE ) )
    {
      addable.add( new LayerDescriptor( CrossSectionTheme.TITLE, IWspmLayers.LAYER_GELAENDE ) );
    }
    if( Objects.isNull( profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_HOCHWERT ) ) && !ArrayUtils.contains( existing, IWspmLayers.LAYER_GEOKOORDINATEN ) )
    {
      addable.add( new LayerDescriptor( GeoCoordinateTheme.TITLE, IWspmLayers.LAYER_GEOKOORDINATEN ) );
    }

    if( profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_COMMENT ) == null )
      addable.add( new LayerDescriptor( CommentTheme.TITLE, IWspmConstants.LAYER_COMMENT ) );

    if( Objects.isNull( profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_CODE ) ) )
      addable.add( new LayerDescriptor( CodeTheme.TITLE, IWspmConstants.LAYER_CODE ) );

    final ISinuositaetProfileObject[] sinObj = profile.getProfileObjects( ISinuositaetProfileObject.class );
    if( sinObj.length < 1 && !ArrayUtils.contains( existing, IWspmTuhhConstants.LAYER_SINUOSITAET ) )
    {
      addable.add( new LayerDescriptor( Messages.getString( "ProfilLayerProviderTuhh.3" ), IWspmTuhhConstants.LAYER_SINUOSITAET ) ); //$NON-NLS-1$
    }

    final IEnergylossProfileObject[] elpo = profile.getProfileObjects( IEnergylossProfileObject.class );
    if( elpo.length < 1 && !ArrayUtils.contains( existing, IWspmTuhhConstants.LAYER_ENERGYLOSS ) )
    {
      addable.add( new LayerDescriptor( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.7" ), IWspmTuhhConstants.LAYER_ENERGYLOSS ) ); //$NON-NLS-1$
    }

    return addable.toArray( new LayerDescriptor[addable.size()] );
  }

  @Override
  public IComponentUiHandlerProvider getComponentUiHandlerProvider( final IProfile profile )
  {
    return new WspmTableUiHandlerProvider( profile );
  }

  public IAxis getDomainAxis( )
  {
    return m_domainAxis;
  }

  public LayerStyleProviderTuhh getLsp( )
  {
    return m_styleProvider;
  }

  public IAxis getTargetAxisLeft( )
  {
    return m_targetAxisLeft;
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider#getChartAxis()
   */
  @Override
  public IAxis[] registerAxis( final IMapperRegistry mapperRegistry )
  {
    if( mapperRegistry == null )
      return new IAxis[] {};

    mapperRegistry.addMapper( m_domainAxis );
    mapperRegistry.addMapper( m_targetAxisLeft );
    mapperRegistry.addMapper( m_targetAxisRight );
    mapperRegistry.addMapper( m_screenAxisVertical );

    // setAxisLabel()

    return new IAxis[] { m_domainAxis, m_targetAxisLeft, m_targetAxisRight, m_screenAxisVertical };
  }

  final void setAxisLabel( final IProfile profil )
  {
    final String domLabel = String.format( AXIS_LABEL, ComponentUtilities.getComponentUnitLabel( profil.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BREITE ) ) );
    m_domainAxis.clearLabels();
    m_domainAxis.addLabel( ChartLabelRendererFactory.getAxisLabelType( m_domainAxis.getPosition(), domLabel, new Insets( 1, 2, 4, 2 ), StyleUtils.getDefaultTextStyle() ) );
    final String leftLabel = String.format( AXIS_LABEL, ComponentUtilities.getComponentUnitLabel( profil.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_HOEHE ) ) );
    m_targetAxisLeft.clearLabels();
    m_targetAxisLeft.addLabel( ChartLabelRendererFactory.getAxisLabelType( m_targetAxisLeft.getPosition(), leftLabel, new Insets( 1, 2, 4, 2 ), StyleUtils.getDefaultTextStyle() ) );

    final IComponent roughnessKS = profil.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS );
    final IComponent roughnessKST = profil.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST );
    final String rightLabel;
    if( roughnessKS != null )
    {
      rightLabel = String.format( AXIS_LABEL, ComponentUtilities.getComponentUnitLabel( roughnessKS ) );
    }
    else if( roughnessKST != null )
    {
      rightLabel = String.format( AXIS_LABEL, ComponentUtilities.getComponentUnitLabel( roughnessKST ) );
    }
    else
    {
      rightLabel = ""; //$NON-NLS-1$
    }
    m_targetAxisRight.clearLabels();
    m_targetAxisRight.addLabel( ChartLabelRendererFactory.getAxisLabelType( m_targetAxisRight.getPosition(), rightLabel, new Insets( 1, 2, 4, 2 ), StyleUtils.getDefaultTextStyle() ) );
  }
}