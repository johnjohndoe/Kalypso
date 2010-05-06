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
import java.util.Collections;
import java.util.List;

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyAdd;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.changes.ProfileObjectSet;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingKreis;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.view.chart.ComponentLayer;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;
import org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider;
import org.kalypso.model.wspm.ui.view.chart.LayerDescriptor;
import org.kalypso.model.wspm.ui.view.chart.PointsLineLayer;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;
import org.kalypso.model.wspm.ui.view.table.GenericComponentUiHandlerProvider;
import org.kalypso.observation.phenomenon.IPhenomenon;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandlerProvider;

import de.openali.odysseus.chart.ext.base.axis.GenericLinearAxis;
import de.openali.odysseus.chart.ext.base.axisrenderer.AxisRendererConfig;
import de.openali.odysseus.chart.ext.base.axisrenderer.GenericAxisRenderer;
import de.openali.odysseus.chart.ext.base.axisrenderer.GenericNumberTickCalculator;
import de.openali.odysseus.chart.ext.base.axisrenderer.NumberLabelCreator;
import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.model.layer.ILayerManager;
import de.openali.odysseus.chart.framework.model.mapper.IAxis;
import de.openali.odysseus.chart.framework.model.mapper.IAxisConstants.POSITION;
import de.openali.odysseus.chart.framework.model.mapper.impl.AxisAdjustment;
import de.openali.odysseus.chart.framework.model.mapper.impl.CoordinateMapper;
import de.openali.odysseus.chart.framework.model.mapper.registry.IMapperRegistry;
import de.openali.odysseus.chart.framework.model.mapper.renderer.IAxisRenderer;

/**
 * @author kimwerner
 */
public class ProfilLayerProviderTuhh implements IProfilLayerProvider, IWspmTuhhConstants
{
  private final List<String> m_layers = new ArrayList<String>();

  protected final LayerStyleProviderTuhh m_lsp = new LayerStyleProviderTuhh();

  protected final IAxis m_domainAxis = new GenericLinearAxis( "ProfilLayerProviderTuhh_AXIS_DOMAIN", POSITION.BOTTOM, null );//$NON-NLS-1$

  protected final IAxis m_targetAxisLeft = new GenericLinearAxis( "ProfilLayerProviderTuhh_AXIS_LEFT", POSITION.LEFT, null );//$NON-NLS-1$

  protected final IAxis m_targetAxisRight = new GenericLinearAxis( "ProfilLayerProviderTuhh_AXIS_RIGHT", POSITION.RIGHT, null );//$NON-NLS-1$

  private final String m_AxisLabel = "[%s]"; //$NON-NLS-1$

  public ProfilLayerProviderTuhh( )
  {
    m_layers.add( IWspmTuhhConstants.LAYER_BEWUCHS );
    m_layers.add( IWspmTuhhConstants.LAYER_GEOKOORDINATEN );
    m_layers.add( IWspmTuhhConstants.LAYER_GELAENDE );
    // TODO Kim m_layers.add( IWspmTuhhConstants.LAYER_WASSERSPIEGEL );
    m_layers.add( IWspmTuhhConstants.LAYER_RAUHEIT );
    m_layers.add( IWspmTuhhConstants.LAYER_BRUECKE );
    m_layers.add( IWspmTuhhConstants.LAYER_WEHR );
    m_layers.add( IWspmTuhhConstants.LAYER_TUBES );
    m_layers.add( IWspmTuhhConstants.LAYER_DEVIDER );
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider#createLayer()
   */
  public void addLayerToChart( final ProfilChartView view, final String layerId )
  {
    final IProfil profil = view.getProfil();
    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profil.getType() );

    if( layerId.equals( IWspmTuhhConstants.LAYER_BEWUCHS ) )
    {
      final IProfilChange[] changes = new IProfilChange[3];
      changes[0] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX ), 0.0 );
      changes[1] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AY ), 0.0 );
      changes[2] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_DP ), 0.0 );
      final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.0" ), view.getProfil(), changes, true ); //$NON-NLS-1$
      new ProfilOperationJob( operation ).schedule();
    }
    if( layerId.equals( IWspmTuhhConstants.LAYER_GEOKOORDINATEN ) )
    {
      final IProfilChange[] changes = new IProfilChange[2];
      changes[0] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT ) );
      changes[1] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT ) );
      final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.1" ), view.getProfil(), changes, true ); //$NON-NLS-1$
      new ProfilOperationJob( operation ).schedule();
    }
    if( layerId.equals( IWspmTuhhConstants.LAYER_GELAENDE ) )
    {
      final IProfilChange[] changes = new IProfilChange[2];
      changes[0] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE ) );
      changes[1] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmConstants.POINT_PROPERTY_BREITE ) );
      final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.2" ), view.getProfil(), changes, true ); //$NON-NLS-1$
      new ProfilOperationJob( operation ).schedule();
    }

    if( layerId.equals( IWspmTuhhConstants.LAYER_RAUHEIT ) )
    {
      final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.3" ), view.getProfil(), true ); //$NON-NLS-1$
      final IComponent rauheit_kst = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST );
      final IComponent rauheit_ks = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS );
      final IComponent rauheit_neu;
      final IComponent rauheit_alt;
      final Object[] values;

      if( rauheit_ks == null && rauheit_kst == null )
      {
        rauheit_neu = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS );
        rauheit_alt = null;
        values = new Object[] { 0.0 };
      }
      else
      {
        rauheit_alt = rauheit_kst == null ? rauheit_ks : provider.getPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST );
        rauheit_neu = rauheit_ks == null ? rauheit_kst : provider.getPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS );
        values = ProfilUtil.getValuesFor( profil, rauheit_alt );
        operation.addChange( new PointPropertyRemove( profil, rauheit_alt ) );

      }
      m_targetAxisRight.setLabel( String.format( m_AxisLabel, ComponentUtilities.getComponentUnitLabel( rauheit_neu ) ) );
      operation.addChange( new PointPropertyAdd( profil, rauheit_neu, values ) );
      new ProfilOperationJob( operation ).schedule();
    }

    if( layerId.equals( IWspmTuhhConstants.LAYER_BRUECKE ) )
    {
      final IProfilChange[] changes = new IProfilChange[1];
      changes[0] = new ProfileObjectSet( profil, new IProfileObject[] { new BuildingBruecke( profil ) } );

      final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.4" ), view.getProfil(), changes, true ); //$NON-NLS-1$
      new ProfilOperationJob( operation ).schedule();
    }
    if( layerId.equals( IWspmTuhhConstants.LAYER_WEHR ) )
    {
      final IProfilChange[] changes = new IProfilChange[1];
      changes[0] = new ProfileObjectSet( profil, new IProfileObject[] { new BuildingWehr( profil ) } );

      final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.5" ), view.getProfil(), changes, true ); //$NON-NLS-1$
      new ProfilOperationJob( operation ).schedule();
    }
    if( layerId.equals( IWspmTuhhConstants.LAYER_TUBES ) )
    {
      final IProfileObject building = new BuildingKreis( profil );
      final IProfilChange[] changes = new IProfilChange[1];
      changes[0] = new ProfileObjectSet( profil, new IProfileObject[] { building } );
      final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.6" ), view.getProfil(), changes, true ); //$NON-NLS-1$
      new ProfilOperationJob( operation ).schedule();
    }
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider#createLayers(org.kalypso.model.wspm.core.profil.IProfil)
   */
  @Override
  public IProfilChartLayer[] createLayers( final ProfilChartView chartView )
  {
    final IProfil profil = chartView.getProfil();

    final List<IProfilChartLayer> layerToAdd = new ArrayList<IProfilChartLayer>();

    if( profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST ) != null || profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS ) != null )
    {
      final CoordinateMapper cmRight = new CoordinateMapper( m_domainAxis, m_targetAxisRight );

      final IProfilChartLayer[] subLayers = new IProfilChartLayer[] { new RoughnessLayer( profil, IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KST, m_lsp ),
          new RoughnessLayer( profil, IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KS, m_lsp ) };
      final RoughnessTheme roughnessLayer = new RoughnessTheme( profil, subLayers, cmRight );
      layerToAdd.add( roughnessLayer );
    }

    if( profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX ) != null )
    {
      final CoordinateMapper cmLeft = new CoordinateMapper( m_domainAxis, m_targetAxisLeft );

      final VegetationTheme vegetationTheme = new VegetationTheme( profil, new IProfilChartLayer[] { new ComponentLayer( profil, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX ),
          new ComponentLayer( profil, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AY ), new ComponentLayer( profil, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_DP ) }, cmLeft, m_lsp );

      layerToAdd.add( vegetationTheme );
    }

    // TODO IProfileObjects now returned as list from IProfile, but we can only handle one IProfileObject (WSPM can't
    // handle more!)
    final IProfileObject[] buildings = profil.getProfileObjects();

    if( buildings.length > 0 )
    {
      final IProfileObject building = buildings[0];
      if( building != null )
      {
        final CoordinateMapper cmLeft = new CoordinateMapper( m_domainAxis, m_targetAxisLeft );

        final IProfilChartLayer buildingTheme;
        if( building.getId().equals( IWspmTuhhConstants.BUILDING_TYP_BRUECKE ) )
        {
          final IProfilChartLayer[] subLayers = new IProfilChartLayer[] { new PointsLineLayer( profil, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, m_lsp ),
              new PointsLineLayer( profil, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, m_lsp ) };
          buildingTheme = new BuildingBridgeTheme( profil, subLayers, cmLeft );
        }
        else if( building.getId().equals( IWspmTuhhConstants.BUILDING_TYP_WEHR ) )
        {
          final IProfilChartLayer[] subLayers = new IProfilChartLayer[] { new PointsLineLayer( profil, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, m_lsp ),
              new PointMarkerLayer( profil, IWspmTuhhConstants.MARKER_TYP_WEHR, m_lsp, 30, false ) };
          buildingTheme = new BuildingWeirTheme( profil, subLayers, cmLeft );
        }
        else
        {
          buildingTheme = new BuildingTubesTheme( profil, new IProfilChartLayer[] { new TubeLayer( profil, m_lsp ) }, cmLeft );
        }

        layerToAdd.add( buildingTheme );
      }
    }

    if( profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT ) != null )
    {
      final IProfilChartLayer[] subLayers = new IProfilChartLayer[] { new ComponentLayer( profil, IWspmTuhhConstants.POINT_PROPERTY_HOCHWERT ),
          new ComponentLayer( profil, IWspmTuhhConstants.POINT_PROPERTY_RECHTSWERT ) };
      final GeoCoordinateTheme geoCoordinateTheme = new GeoCoordinateTheme( profil, subLayers, null );

      layerToAdd.add( geoCoordinateTheme );
    }

    if( profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE ) != null )
    {
      final CoordinateMapper cmLeft = new CoordinateMapper( m_domainAxis, m_targetAxisLeft );

      final CrossSectionTheme crossSectionTheme = new CrossSectionTheme( profil, new IProfilChartLayer[] { new StationLineLayer( profil, IWspmConstants.POINT_PROPERTY_HOEHE ),
          new PointsLineLayer( profil, IWspmConstants.POINT_PROPERTY_HOEHE, m_lsp ) }, cmLeft );

      layerToAdd.add( crossSectionTheme );
    }

    final IComponent[] pointProperties = profil.getPointProperties();
    for( int i = 0; i < pointProperties.length; i++ )
    {
      final IComponent property = pointProperties[i];
      final IPhenomenon phenomenon = property.getPhenomenon();

      if( IWspmConstants.PHENOMENON_WATERLEVEL_2D.equals( phenomenon.getID() ) )
      {
        final PointsLineLayer layer = new PointsLineLayer( LAYER_WASSERSPIEGEL2D, profil, i, m_lsp );
        layer.setTitle( property.getName() );

        layer.setCoordinateMapper( new CoordinateMapper( m_domainAxis, m_targetAxisLeft ) );
        layerToAdd.add( layer );
      }
    }

    layerToAdd.add( createTrennerLayer( profil ) );
    layerToAdd.add( createWspLayer( profil ) );

    setAxisLabel( profil );

    /* Prune 'null's returned from createLayer-subroutines */
    layerToAdd.removeAll( Collections.singleton( null ) );

    return layerToAdd.toArray( new IProfilChartLayer[layerToAdd.size()] );
  }

  /* We always have a trenner layer, even if no trenner is defined. */
  private IProfilChartLayer createTrennerLayer( final IProfil profil )
  {
    final CoordinateMapper cmLeft = new CoordinateMapper( m_domainAxis, m_targetAxisLeft );

    final PointMarkerLayer dbLayer = new PointMarkerLayer( profil, IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, m_lsp, 5, true );
    final PointMarkerLayer bvLayer = new PointMarkerLayer( profil, IWspmTuhhConstants.MARKER_TYP_BORDVOLL, m_lsp, 25, false );
    final RiverChannelLayer tfLayer = new RiverChannelLayer( profil, m_lsp, 15, false );
    final IProfilChartLayer[] subLayers = new IProfilChartLayer[] { dbLayer, bvLayer, tfLayer };
    return new DeviderTheme( profil, subLayers, cmLeft );
  }

  private IProfilChartLayer createWspLayer( final IProfil profil )
  {
    // TODO Auto-generated method stub
    return null;
  }

  final void setAxisLabel( final IProfil profil )
  {
    m_domainAxis.setLabel( String.format( m_AxisLabel, ComponentUtilities.getComponentUnitLabel( profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BREITE ) ) ) );
    m_targetAxisLeft.setLabel( String.format( m_AxisLabel, ComponentUtilities.getComponentUnitLabel( profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE ) ) ) );
    final IComponent roughnessKS = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS );
    final IComponent roughnessKST = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST );
    if( roughnessKS != null )
      m_targetAxisRight.setLabel( String.format( m_AxisLabel, ComponentUtilities.getComponentUnitLabel( roughnessKS ) ) );
    else if( roughnessKST != null )
      m_targetAxisRight.setLabel( String.format( m_AxisLabel, ComponentUtilities.getComponentUnitLabel( roughnessKST ) ) );
    else
      m_targetAxisRight.setLabel( "" ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider#getAddableLayers(org.kalypso.model.wspm.ui.view.chart.ProfilChartView)
   */
  public LayerDescriptor[] getAddableLayers( final ProfilChartView view )
  {
    final List<LayerDescriptor> addableLayer = new ArrayList<LayerDescriptor>();

    final List<String> existingLayers = new ArrayList<String>();

    final ILayerManager mngr = view.getChart().getChartModel().getLayerManager();
    final IProfil profile = view.getProfil();
    if( mngr == null || profile == null )
      return new LayerDescriptor[] {};

    for( final IChartLayer layer : mngr.getLayers() )
      existingLayers.add( layer.getId() );

    // only ONE Object allowed
    if( profile.getProfileObjects().length == 0 )
    {
      addableLayer.add( new LayerDescriptor( BuildingBridgeTheme.TITLE, LAYER_BRUECKE ) );
      addableLayer.add( new LayerDescriptor( BuildingWeirTheme.TITLE, IWspmTuhhConstants.LAYER_WEHR ) );
      addableLayer.add( new LayerDescriptor( BuildingTubesTheme.TITLE, IWspmTuhhConstants.LAYER_TUBES ) );
    }
    // always show devider and roughness
    if( !existingLayers.contains( IWspmTuhhConstants.LAYER_RAUHEIT ) )
      addableLayer.add( new LayerDescriptor( RoughnessTheme.TITLE, IWspmTuhhConstants.LAYER_RAUHEIT ) );
    if( !existingLayers.contains( IWspmTuhhConstants.LAYER_DEVIDER ) )
      addableLayer.add( new LayerDescriptor( DeviderTheme.TITLE, IWspmTuhhConstants.LAYER_DEVIDER ) );

    if( profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX ) == null && !existingLayers.contains( IWspmTuhhConstants.LAYER_BEWUCHS ) )
      addableLayer.add( new LayerDescriptor( VegetationTheme.TITLE, IWspmTuhhConstants.LAYER_BEWUCHS ) );
    if( profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE ) == null && !existingLayers.contains( IWspmTuhhConstants.LAYER_GELAENDE ) )
      addableLayer.add( new LayerDescriptor( CrossSectionTheme.TITLE, IWspmTuhhConstants.LAYER_GELAENDE ) );
    if( profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT ) == null && !existingLayers.contains( IWspmTuhhConstants.LAYER_GEOKOORDINATEN ) )
      addableLayer.add( new LayerDescriptor( GeoCoordinateTheme.TITLE, IWspmTuhhConstants.LAYER_GEOKOORDINATEN ) );

    return addableLayer.toArray( new LayerDescriptor[addableLayer.size()] );
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider#getAxisRenderer(de.openali.odysseus.chart.framework.model.mapper.IAxis[])
   */
  @Override
  public IAxisRenderer[] registerAxisRenderer( final IMapperRegistry mapperRegistry )
  {
    if( mapperRegistry == null )
      return new IAxisRenderer[] {};
    final IAxisRenderer aRendDom = new GenericAxisRenderer( "ProfilLayerProviderTuhh_AXIS_DOMAIN_RENDERER", new NumberLabelCreator( "%s" ), new GenericNumberTickCalculator(), new AxisRendererConfig() ); //$NON-NLS-1$ //$NON-NLS-2$

    final AxisRendererConfig configLR = new AxisRendererConfig();
    configLR.gap = 5;
    final IAxisRenderer aRendLR = new GenericAxisRenderer( "ProfilLayerProviderTuhh_AXIS_TARGET_RENDERER", new NumberLabelCreator( "%s" ), new GenericNumberTickCalculator(), configLR ); //$NON-NLS-1$ //$NON-NLS-2$

    mapperRegistry.setRenderer( "ProfilLayerProviderTuhh_AXIS_DOMAIN", aRendDom );//$NON-NLS-1$
    mapperRegistry.setRenderer( "ProfilLayerProviderTuhh_AXIS_LEFT", aRendLR );//$NON-NLS-1$
    mapperRegistry.setRenderer( "ProfilLayerProviderTuhh_AXIS_RIGHT", aRendLR );//$NON-NLS-1$

    return new IAxisRenderer[] { aRendDom, aRendLR };
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider#getChartAxis()
   */
  @Override
  public IAxis[] registerAxis( final IMapperRegistry mapperRegistry )
  {
    if( mapperRegistry == null )
      return new IAxis[] {};

    final AxisAdjustment aaDom = new AxisAdjustment( 3, 94, 3 );
    m_domainAxis.setPreferredAdjustment( aaDom );

    final AxisAdjustment aaLeft = new AxisAdjustment( 15, 75, 10 );
    m_targetAxisLeft.setPreferredAdjustment( aaLeft );

    final AxisAdjustment aaRight = new AxisAdjustment( 2, 40, 58 );
    m_targetAxisRight.setPreferredAdjustment( aaRight );

    mapperRegistry.addMapper( m_domainAxis );
    mapperRegistry.addMapper( m_targetAxisLeft );
    mapperRegistry.addMapper( m_targetAxisRight );

    return new IAxis[] { m_domainAxis, m_targetAxisLeft, m_targetAxisRight };
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider#getComponentUiHandlerProvider()
   */
  public IComponentUiHandlerProvider getComponentUiHandlerProvider( final IProfil profile )
  {
    return new GenericComponentUiHandlerProvider( profile );
  }
}
