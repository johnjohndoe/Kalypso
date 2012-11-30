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
import org.kalypso.model.wspm.core.gml.IProfileSelection;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfilePointPropertyProvider;
import org.kalypso.model.wspm.core.profil.impl.GenericProfileHorizon;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.energyloss.IEnergylossProfileObject;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.ICulvertBuilding;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.IProfileBuilding;
import org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.ISinuositaetProfileObject;
import org.kalypso.model.wspm.tuhh.core.profile.utils.TuhhProfiles;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode;
import org.kalypso.model.wspm.tuhh.ui.chart.layers.EnergylossLayer;
import org.kalypso.model.wspm.tuhh.ui.chart.layers.ProfileObjectsLayer;
import org.kalypso.model.wspm.tuhh.ui.chart.layers.SecondProfileDataLayer;
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
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;
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

    // FIXME: strange?! axis created per provider?? should be per chart....
    m_screenAxisVertical = new ScreenCoordinateAxis( "ProfilLayerProviderTuhh_AXIS_VERTICAL_SCREEN", POSITION.RIGHT );//$NON-NLS-1$
    m_screenAxisVertical.setPreferredAdjustment( new AxisAdjustment( 0, 1, 0 ) );
    m_domainAxis = new GenericLinearAxis( "ProfilLayerProviderTuhh_AXIS_DOMAIN", POSITION.BOTTOM, axisRendererConfigD );//$NON-NLS-1$
    m_domainAxis.setPreferredAdjustment( new AxisAdjustment( 1, 98, 1 ) );
    m_targetAxisLeft = new GenericLinearAxis( "ProfilLayerProviderTuhh_AXIS_LEFT", POSITION.LEFT, axisRendererConfigLR );//$NON-NLS-1$
    m_targetAxisLeft.setPreferredAdjustment( new AxisAdjustment( 15, 80, 5 ) );
    m_targetAxisRight = new GenericLinearAxis( "ProfilLayerProviderTuhh_AXIS_RIGHT", POSITION.RIGHT, axisRendererConfigLR );//$NON-NLS-1$
    m_targetAxisRight.setPreferredAdjustment( new AxisAdjustment( 2, 25, 73 ) );
  }

  // FIXME: once, this was nice and object oriented; we should do it again like this: encapsulate adding layer into a class! -> this class is presented in the ui
  @Override
  public void addLayerToProfile( final Shell shell, final IProfile profil, final String layerId )
  {
    final IProfilePointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profil.getType() );

    switch( layerId )
    {
      case IWspmTuhhConstants.LAYER_BEWUCHS:
        TuhhLayersAdder.addVegetationLayer( provider, profil );
        break;

      case IWspmLayers.LAYER_GEOKOORDINATEN:
        TuhhLayersAdder.addGeoLayer( provider, profil );
        break;

      case IWspmLayers.THEME_GELAENDE:
        TuhhLayersAdder.addGroundLayer( provider, profil );
        break;

      case IWspmTuhhConstants.LAYER_RAUHEIT:
        TuhhLayersAdder.addRoughnessLayers( provider, profil, m_targetAxisRight, AXIS_LABEL );
        break;

      case IWspmTuhhConstants.THEME_BRUECKE:
        TuhhLayersAdder.addBridgeLayer( shell, profil );
        break;

      case IWspmTuhhConstants.THEME_WEHR:
        TuhhLayersAdder.addWeirLayer( shell, profil );
        break;

      case IWspmTuhhConstants.THEME_TUBES:
        TuhhLayersAdder.addTubesLayer( shell, profil );
        break;

      case IWspmTuhhConstants.LAYER_SINUOSITAET:
        TuhhLayersAdder.addSinuositaetLayer( profil );
        break;

      case IWspmTuhhConstants.LAYER_ENERGYLOSS:
        TuhhLayersAdder.addEnergylossLayer( profil );
        break;

      case IWspmTuhhConstants.LAYER_COMMENT:
        TuhhLayersAdder.addCommentLayer( provider, profil );
        break;

      case IWspmLayers.LAYER_CODE:
        TuhhLayersAdder.addCodeLayer( provider, profil );
        break;

      case IWspmLayers.LAYER_SECOND_PROFILE:
        TuhhLayersAdder.addSeccondProfileLayer( provider, profil );
        break;

      case IWspmLayers.LAYER_PROFILE_OBJECTS:
        TuhhLayersAdder.addSeccondProfileLayer( provider, profil );
        break;
    }
  }

  @Override
  public IProfilChartLayer[] createLayers( final IProfile profile, final Object result )
  {
    final CoordinateMapper cmLeft = new CoordinateMapper( m_domainAxis, m_targetAxisLeft );
    final CoordinateMapper cmRight = new CoordinateMapper( m_domainAxis, m_targetAxisRight );
    final CoordinateMapper cmScreen = new CoordinateMapper( m_domainAxis, m_screenAxisVertical );

    // Achtung: diese Reihenfolge ist die natürliche Ordnung im Layermanager
    final List<IProfilChartLayer> layersToAdd = new ArrayList<>();

    /* water level layer */
    final IProfilChartLayer wspLayer = TuhhLayers.createWspLayer( profile, (IWspmResultNode)result, cmLeft, m_styleProvider );
    if( wspLayer != null )
      layersToAdd.add( wspLayer );

    /* water level fixation layer */
    final IProfilChartLayer wspFixationLayer = TuhhLayers.createWspFixationLayer( profile, (IWspmResultNode)result, cmLeft, m_styleProvider );
    if( wspFixationLayer != null )
      layersToAdd.add( wspFixationLayer );

    /* roughnesses */
    if( TuhhProfiles.hasRoughness( profile ) )
      layersToAdd.add( TuhhLayers.createRoughnessLayer( profile, cmRight, m_styleProvider ) );

    /* vegetation layer */
    if( TuhhProfiles.hasVegetation( profile ) )
      layersToAdd.add( TuhhLayers.createVegetationLayer( profile, cmLeft, m_styleProvider ) );

    /* profile objects, not handled by other layers */
    final IProfileObject[] profileObjects = profile.getProfileObjects();
    int objectLayer = 0;
    for( final IProfileObject profileObject : profileObjects )
    {
      if( shouldHaveOwnLayer( profile, profileObject ) )
      {
        final String id = profileObject.getType() + objectLayer++;

        final ProfileObjectsLayer subLayer = new ProfileObjectsLayer( id, profile, profileObject, null );
        subLayer.setCoordinateMapper( cmLeft );

        layersToAdd.add( subLayer );
      }
    }

    /* profile buildings layer(s) */
    final IProfileObject[] buildings = profile.getProfileObjects( IProfileBuilding.class );
    for( final IProfileObject building : buildings )
      layersToAdd.add( createBuildingLayer( profile, building, cmLeft, cmScreen ) );

    /* 2d layers */
    final IProfilChartLayer[] twoDLayers = TuhhLayers.create2DWaterLevelLayers( profile, m_domainAxis, m_targetAxisLeft, m_styleProvider );
    Collections.addAll( layersToAdd, twoDLayers );

    /* ground layer */
    if( profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_HOEHE ) != null )
      layersToAdd.add( TuhhLayers.createGelaendeLayer( profile, cmLeft, m_styleProvider ) );

    /* profile deviders (trennflächen) */
    layersToAdd.add( TuhhLayers.createDeviderLayer( profile, cmScreen, m_styleProvider ) );

    /* second profile layers */
    final SecondProfileDataManager secondProfileManager = SecondProfileDataManager.instance();
    final SecondProfileData[] secondProfileData = secondProfileManager.findData( profile );
    for( final SecondProfileData data : secondProfileData )
      layersToAdd.add( new SecondProfileDataLayer( profile, data, cmLeft ) );

    /* sinuosität */
    final ISinuositaetProfileObject[] sinObj = profile.getProfileObjects( ISinuositaetProfileObject.class );
    if( ArrayUtils.isNotEmpty( sinObj ) )
      layersToAdd.add( new SinuositaetLayer( profile ) );

    /* energyloss */
    final IEnergylossProfileObject[] elpo = profile.getProfileObjects( IEnergylossProfileObject.class );
    if( ArrayUtils.isNotEmpty( elpo ) )
      layersToAdd.add( new EnergylossLayer( profile ) );

    /* geo coordinates */
    if( profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_HOCHWERT ) != null )
      layersToAdd.add( new GeoCoordinateTheme( profile ) );

    /* code */
    if( profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_CODE ) != null )
      layersToAdd.add( new CodeTheme( profile ) );

    /* comment */
    if( profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_COMMENT ) != null )
      layersToAdd.add( new CommentTheme( profile ) );

    /* Prune 'null's returned from createLayer-subroutines */
    layersToAdd.removeAll( Collections.singleton( null ) );

    // TODO why here ?
    setAxisLabel( profile );

    return layersToAdd.toArray( new IProfilChartLayer[layersToAdd.size()] );
  }

  private IProfilChartLayer createBuildingLayer( final IProfile profile, final IProfileObject building, final ICoordinateMapper cmLeft, final ICoordinateMapper cmScreen )
  {
    if( building instanceof BuildingBruecke )
      return TuhhLayers.createBridgeLayer( profile, cmLeft, m_styleProvider );

    if( building instanceof BuildingWehr )
      return TuhhLayers.createWehrLayer( profile, cmLeft, cmScreen, m_styleProvider );

    if( building instanceof ICulvertBuilding )
      return new BuildingTubesTheme( profile, (ICulvertBuilding)building, cmLeft, m_styleProvider );

    return null;
  }

  @Override
  public LayerDescriptor[] getAddableLayers( final ProfilChartModel chartModel )
  {
    final IProfileSelection profilSelection = chartModel.getProfileSelection();
    if( profilSelection == null )
      return new LayerDescriptor[] {};

    final IProfile profile = profilSelection.getProfile();
    if( Objects.isNull( profile ) )
      return new LayerDescriptor[] {};

    final List<LayerDescriptor> addable = new ArrayList<>();
    final String[] existing = TuhhLayers.getExistingLayers( chartModel );

    final IProfileObject[] objects = profile.getProfileObjects( IProfileBuilding.class );

    // only ONE Object allowed
    if( ArrayUtils.isEmpty( objects ) )
    {
      addable.add( new LayerDescriptor( BuildingBridgeTheme.TITLE, THEME_BRUECKE ) );
      addable.add( new LayerDescriptor( BuildingWeirTheme.TITLE, IWspmTuhhConstants.THEME_WEHR ) );
      addable.add( new LayerDescriptor( BuildingTubesTheme.TITLE, IWspmTuhhConstants.THEME_TUBES ) );
    }

    // FIXME: nonse: layers to add decided by already existing layers? should only depend on profile data!

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
    if( Objects.isNull( profile.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_HOEHE ) ) && !ArrayUtils.contains( existing, IWspmLayers.THEME_GELAENDE ) )
    {
      addable.add( new LayerDescriptor( CrossSectionTheme.TITLE, IWspmLayers.THEME_GELAENDE ) );
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

    // FIXME
    // addable.add( new LayerDescriptor( "Profil einblenden", IWspmTuhhConstants.LAYER_SECOND_PROFILE ) );

    return addable.toArray( new LayerDescriptor[addable.size()] );
  }

  @Override
  public IComponentUiHandlerProvider getComponentUiHandlerProvider( final IProfile profile )
  {
    return new WspmTableUiHandlerProvider( profile );
  }

  protected IAxis getDomainAxis( )
  {
    return m_domainAxis;
  }

  protected LayerStyleProviderTuhh getLsp( )
  {
    return m_styleProvider;
  }

  protected IAxis getTargetAxisLeft( )
  {
    return m_targetAxisLeft;
  }

  @Override
  public IAxis[] registerAxis( final IMapperRegistry mapperRegistry )
  {
    if( mapperRegistry == null )
      return new IAxis[] {};

    mapperRegistry.addMapper( m_domainAxis );
    mapperRegistry.addMapper( m_targetAxisLeft );
    mapperRegistry.addMapper( m_targetAxisRight );
    mapperRegistry.addMapper( m_screenAxisVertical );

    return new IAxis[] { m_domainAxis, m_targetAxisLeft, m_targetAxisRight, m_screenAxisVertical };
  }

  private final void setAxisLabel( final IProfile profil )
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

  private static boolean shouldHaveOwnLayer( final IProfile profile, final IProfileObject profileObject )
  {
    // ignore objects that have own specialized layers and will never have own records

    if( profileObject instanceof IProfileBuilding )
      return false;

    if( profileObject instanceof ISinuositaetProfileObject )
      return false;

    if( profileObject instanceof IEnergylossProfileObject )
      return false;

    if( profileObject instanceof GenericProfileHorizon )
    {
      final GenericProfileHorizon horizon = (GenericProfileHorizon)profileObject;
      final String type = horizon.getType();

      if( IWspmTuhhConstants.OBJECT_TYPE_WATERLEVEL_POINTS.equals( type ) )
        return false;
      if( IWspmTuhhConstants.OBJECT_TYPE_WATERLEVEL_SEGMENT.equals( type ) )
        return false;

      if( BuildingBruecke.ID_OK.equals( type ) )
      {
        final BuildingBruecke[] bridges = profile.getProfileObjects( BuildingBruecke.class );
        for( final BuildingBruecke bridge : bridges )
        {
          final IProfileObject bridgeOK = bridge.findOkProfileObject( profile );
          if( bridgeOK == profileObject )
            return false;
        }

        return true;
      }
    }

    return true;
  }
}