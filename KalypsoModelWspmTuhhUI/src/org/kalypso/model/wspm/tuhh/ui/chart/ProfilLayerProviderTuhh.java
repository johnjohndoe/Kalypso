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
import org.kalypso.model.wspm.ui.view.chart.PointsLineLayer;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;
import org.kalypso.model.wspm.ui.view.table.GenericComponentUiHandlerProvider;
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
public class ProfilLayerProviderTuhh implements IProfilLayerProvider
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
  public IProfilChartLayer addLayerToChart( final ProfilChartView view, final String layerId )
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
      return null;// new BewuchsLayer( view );
    }
    if( layerId.equals( IWspmTuhhConstants.LAYER_GEOKOORDINATEN ) )
    {
      final IProfilChange[] changes = new IProfilChange[2];
      changes[0] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT ) );
      changes[1] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT ) );
      final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.1" ), view.getProfil(), changes, true ); //$NON-NLS-1$
      new ProfilOperationJob( operation ).schedule();
      return null;// new HochRechtsLayer( view );
    }
    if( layerId.equals( IWspmTuhhConstants.LAYER_GELAENDE ) )
    {
      final IProfilChange[] changes = new IProfilChange[2];
      changes[0] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE ) );
      changes[1] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmConstants.POINT_PROPERTY_BREITE ) );
      final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.2" ), view.getProfil(), changes, true ); //$NON-NLS-1$
      new ProfilOperationJob( operation ).schedule();
      return null;// new GelaendeLayer( view );
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
      return null;
    }

    if( layerId.equals( IWspmTuhhConstants.LAYER_BRUECKE ) )
    {
      final IProfilChange[] changes = new IProfilChange[1];
      changes[0] = new ProfileObjectSet( profil, new IProfileObject[] { new BuildingBruecke( profil ) } );

      final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.4" ), view.getProfil(), changes, true ); //$NON-NLS-1$
      new ProfilOperationJob( operation ).schedule();
      return null;// new BrueckeBuildingLayer( view );
    }
    if( layerId.equals( IWspmTuhhConstants.LAYER_WEHR ) )
    {
      final IProfilChange[] changes = new IProfilChange[1];
      changes[0] = new ProfileObjectSet( profil, new IProfileObject[] { new BuildingWehr( profil ) } );

      final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.5" ), view.getProfil(), changes, true ); //$NON-NLS-1$
      new ProfilOperationJob( operation ).schedule();
      return null;// new WehrBuildingLayer( view );
    }
    if( layerId.equals( IWspmTuhhConstants.LAYER_TUBES ) )
    {
      final IProfileObject building = new BuildingKreis( profil );
      final IProfilChange[] changes = new IProfilChange[1];
      changes[0] = new ProfileObjectSet( profil, new IProfileObject[] { building } );
      final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.6" ), view.getProfil(), changes, true ); //$NON-NLS-1$
      new ProfilOperationJob( operation ).schedule();
      return null;
    }

    return createLayer( layerId, view );
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider#getLayer(java.lang.String)
   */
  public IProfilChartLayer createLayer( final String layerId, final ProfilChartView view )
  {
    final CoordinateMapper cmLeft = new CoordinateMapper( m_domainAxis, m_targetAxisLeft );
    final CoordinateMapper cmRight = new CoordinateMapper( m_domainAxis, m_targetAxisRight );

    final IProfil profil = view.getProfil();

    if( layerId.equals( IWspmTuhhConstants.LAYER_BEWUCHS ) )
      return new VegetationTheme( profil, new IProfilChartLayer[] { new ComponentLayer( profil, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX ),
          new ComponentLayer( profil, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AY ), new ComponentLayer( profil, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_DP ) }, cmLeft, m_lsp );

    if( layerId.equals( IWspmTuhhConstants.LAYER_GEOKOORDINATEN ) )
      return new GeoCoordinateTheme( profil, new IProfilChartLayer[] { new ComponentLayer( profil, IWspmTuhhConstants.POINT_PROPERTY_HOCHWERT ),
          new ComponentLayer( profil, IWspmTuhhConstants.POINT_PROPERTY_RECHTSWERT ) }, null );

    if( layerId.equals( IWspmTuhhConstants.LAYER_GELAENDE ) )
      return new CrossSectionTheme( profil, new IProfilChartLayer[] { new StationLineLayer( profil, IWspmConstants.POINT_PROPERTY_HOEHE ),
          new PointsLineLayer( profil, IWspmConstants.POINT_PROPERTY_HOEHE, m_lsp ) }, cmLeft );

    if( layerId.equals( IWspmTuhhConstants.LAYER_DEVIDER ) )
      return new DeviderTheme( profil, new IProfilChartLayer[] { new PointMarkerLayer( profil, IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, m_lsp, 5, true ),
          new PointMarkerLayer( profil, IWspmTuhhConstants.MARKER_TYP_BORDVOLL, m_lsp, 25, false ), new RiverChannelLayer( profil, IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE, m_lsp, 15, false ) }, cmLeft );

    if( layerId.equals( IWspmTuhhConstants.LAYER_RAUHEIT ) )
    {
      return new RoughnessTheme( profil, new IProfilChartLayer[] { new RoughnessLayer( profil, IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KST, m_lsp ),
          new RoughnessLayer( profil, IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KS, m_lsp ) }, cmRight );
    }

    if( layerId.equals( IWspmTuhhConstants.LAYER_BRUECKE ) )
      return new BuildingBridgeTheme( profil, new IProfilChartLayer[] { new PointsLineLayer( profil, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, m_lsp ),
          new PointsLineLayer( profil, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, m_lsp ) }, cmLeft );

    if( layerId.equals( IWspmTuhhConstants.LAYER_WEHR ) )
      return new BuildingWeirTheme( profil, new IProfilChartLayer[] { new PointsLineLayer( profil, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, m_lsp ),
          new PointMarkerLayer( profil, IWspmTuhhConstants.MARKER_TYP_WEHR, m_lsp, 30, false ) }, cmLeft );

    if( layerId.equals( IWspmTuhhConstants.LAYER_TUBES ) )
      return new BuildingTubesTheme( profil, new IProfilChartLayer[] { new TubeLayer( profil, m_lsp ) }, cmLeft );

// if( layerId.equals( IWspmTuhhConstants.LAYER_WASSERSPIEGEL ) )
// {
// final List<IStationResult> resultLayers = new ArrayList<IStationResult>();
// for( final IStationResult result : view.getResults() )
// {
// resultLayers.add( new WspLayer( view, result ) );
// }
// return resultLayers.toArray( new IProfilChartLayer[0] );
// }

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
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider#getAddableLayers()
   */
  public String[] getAddableLayers( final ProfilChartView view )
  {
    final List<String> addableLayer = new ArrayList<String>();
    final List<String> existingLayers = new ArrayList<String>();
    final ILayerManager mngr = view.getChart().getChartModel().getLayerManager();
    final IProfil profile = view.getProfil();
    if( mngr == null || profile == null )
      return new String[] {};

    for( final IChartLayer layer : mngr.getLayers() )
      existingLayers.add( layer.getId() );

    // only ONE Object allowed
    if( profile.getProfileObjects().length == 0 )
    {
      addableLayer.add( IWspmTuhhConstants.LAYER_BRUECKE );
      addableLayer.add( IWspmTuhhConstants.LAYER_WEHR );
      addableLayer.add( IWspmTuhhConstants.LAYER_TUBES );
    }
    // always show devider and roughness
    if( !existingLayers.contains( IWspmTuhhConstants.LAYER_RAUHEIT ) )
      addableLayer.add( IWspmTuhhConstants.LAYER_RAUHEIT );
    if( !existingLayers.contains( IWspmTuhhConstants.LAYER_DEVIDER ) )
      addableLayer.add( IWspmTuhhConstants.LAYER_DEVIDER );

    if( profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX ) == null && !existingLayers.contains( IWspmTuhhConstants.LAYER_BEWUCHS ) )
      addableLayer.add( IWspmTuhhConstants.LAYER_BEWUCHS );
    if( profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE ) == null && !existingLayers.contains( IWspmTuhhConstants.LAYER_GELAENDE ) )
      addableLayer.add( IWspmTuhhConstants.LAYER_GELAENDE );
    if( profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT ) == null && !existingLayers.contains( IWspmTuhhConstants.LAYER_GEOKOORDINATEN ) )
      addableLayer.add( IWspmTuhhConstants.LAYER_GEOKOORDINATEN );

    if( view.getResults().length > 0 && !existingLayers.contains( IWspmTuhhConstants.LAYER_WASSERSPIEGEL ) )
      addableLayer.add( IWspmTuhhConstants.LAYER_WASSERSPIEGEL );

    return addableLayer.toArray( new String[0] );
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

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider#addRequieredLayer(org.kalypso.model.wspm.ui.view.chart.ProfilChartView)
   */
  public String[] getRequiredLayer( final ProfilChartView view )
  {
    final ArrayList<String> layerToAdd = new ArrayList<String>();
    final IProfil profile = view.getProfil();
    if( profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST ) != null || profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS ) != null )
      layerToAdd.add( IWspmTuhhConstants.LAYER_RAUHEIT );
    if( profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX ) != null )
      layerToAdd.add( IWspmTuhhConstants.LAYER_BEWUCHS );

    // TODO IProfileObjects now returned as list from IProfile, but we can only handle one IProfileObject (WSPM can't
    // handle more!)
    final IProfileObject[] buildings = profile.getProfileObjects();

    IProfileObject building = null;
    if( buildings.length > 0 )
      building = buildings[0];

    if( building != null )
    {
      if( building.getId().equals( IWspmTuhhConstants.BUILDING_TYP_BRUECKE ) )
        layerToAdd.add( IWspmTuhhConstants.LAYER_BRUECKE );
      else if( building.getId().equals( IWspmTuhhConstants.BUILDING_TYP_WEHR ) )
        layerToAdd.add( IWspmTuhhConstants.LAYER_WEHR );
      else
        layerToAdd.add( IWspmTuhhConstants.LAYER_TUBES );
    }

    if( profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT ) != null )
      layerToAdd.add( IWspmTuhhConstants.LAYER_GEOKOORDINATEN );

    if( view.getResults().length > 0 )
      layerToAdd.add( IWspmTuhhConstants.LAYER_WASSERSPIEGEL );

    if( profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE ) != null )
      layerToAdd.add( IWspmTuhhConstants.LAYER_GELAENDE );

    /* We always have a trenner layer, even if no trenner is defined. */
    layerToAdd.add( IWspmTuhhConstants.LAYER_DEVIDER );
    setAxisLabel( profile );
    return layerToAdd.toArray( new String[0] );
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider#providesLayer(java.lang.String)
   */
  public boolean providesLayer( final String layerId )
  {
    return m_layers.contains( layerId );
  }

}
