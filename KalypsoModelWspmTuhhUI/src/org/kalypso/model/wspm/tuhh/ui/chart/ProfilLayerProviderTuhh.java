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
package org.kalypso.model.wspm.tuhh.ui.chart;

import java.awt.Insets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.commons.java.lang.Arrays;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.IWspmPhenomenonConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyAdd;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.changes.ProfileObjectAdd;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.Buildings;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.IProfileBuilding;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingKreis;
import org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.ISinuositaetProfileObject;
import org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.SinuositaetProfileObject;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode;
import org.kalypso.model.wspm.tuhh.ui.chart.data.TuhhResultDataProvider;
import org.kalypso.model.wspm.tuhh.ui.chart.layers.PointMarkerLayer;
import org.kalypso.model.wspm.tuhh.ui.chart.layers.RiverChannelLayer;
import org.kalypso.model.wspm.tuhh.ui.chart.layers.RoughnessLayer;
import org.kalypso.model.wspm.tuhh.ui.chart.layers.SinuositaetLayer;
import org.kalypso.model.wspm.tuhh.ui.chart.layers.StationPointLayer;
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
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.view.chart.ComponentLayer;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;
import org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider;
import org.kalypso.model.wspm.ui.view.chart.LayerDescriptor;
import org.kalypso.model.wspm.ui.view.chart.PointsLineLayer;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartModel;
import org.kalypso.model.wspm.ui.view.chart.layer.CrossSectionTheme;
import org.kalypso.model.wspm.ui.view.chart.layer.StationLineLayer;
import org.kalypso.model.wspm.ui.view.chart.layer.wsp.IWspLayerData;
import org.kalypso.model.wspm.ui.view.chart.layer.wsp.WspLayer;
import org.kalypso.model.wspm.ui.view.chart.layer.wspfixation.WspFixationLayer;
import org.kalypso.model.wspm.ui.view.table.GenericComponentUiHandlerProvider;
import org.kalypso.observation.phenomenon.IPhenomenon;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandlerProvider;

import de.openali.odysseus.chart.ext.base.axis.GenericLinearAxis;
import de.openali.odysseus.chart.ext.base.axis.ScreenCoordinateAxis;
import de.openali.odysseus.chart.ext.base.axisrenderer.AxisRendererConfig;
import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.model.layer.ILayerManager;
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
    axisRendererConfigLR.axisInsets = new Insets( 5, 0, 0, 0 );
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

  @Override
  // TODO: ugly; put this add stuff into another abstraction
  public void addLayerToProfile( final IProfil profil, final String layerId )
  {
    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profil.getType() );

    if( layerId.equals( IWspmTuhhConstants.LAYER_BEWUCHS ) )
    {
      final IProfilChange[] changes = new IProfilChange[3];
      changes[0] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX ), 0.0 );
      changes[1] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AY ), 0.0 );
      changes[2] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_DP ), 0.0 );
      final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.0" ), profil, changes, true ); //$NON-NLS-1$
      new ProfilOperationJob( operation ).schedule();
    }

    if( layerId.equals( IWspmConstants.LAYER_GEOKOORDINATEN ) )
    {
      final IProfilChange[] changes = new IProfilChange[2];
      changes[0] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT ) );
      changes[1] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT ) );
      final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.1" ), profil, changes, true ); //$NON-NLS-1$
      new ProfilOperationJob( operation ).schedule();
    }

    if( layerId.equals( IWspmConstants.LAYER_COMMENT ) )
    {
      final IProfilChange[] changes = new IProfilChange[1];
      changes[0] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmConstants.POINT_PROPERTY_COMMENT ) );
      final ProfilOperation operation = new ProfilOperation( "Insert comments", profil, changes, true ); //$NON-NLS-1$
      new ProfilOperationJob( operation ).schedule();
    }

    if( layerId.equals( IWspmConstants.LAYER_CODE ) )
    {
      final IProfilChange[] changes = new IProfilChange[1];
      changes[0] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmConstants.POINT_PROPERTY_CODE ) );
      final ProfilOperation operation = new ProfilOperation( "Insert code", profil, changes, true ); //$NON-NLS-1$
      new ProfilOperationJob( operation ).schedule();
    }

    if( layerId.equals( IWspmConstants.LAYER_GELAENDE ) )
    {
      final IProfilChange[] changes = new IProfilChange[2];
      changes[0] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE ) );
      changes[1] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmConstants.POINT_PROPERTY_BREITE ) );
      final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.2" ), profil, changes, true ); //$NON-NLS-1$
      new ProfilOperationJob( operation ).schedule();
    }

    if( layerId.equals( IWspmTuhhConstants.LAYER_RAUHEIT ) )
    {
      final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.3" ), profil, true ); //$NON-NLS-1$
      final IComponent rauheitKst = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST );
      final IComponent rauheitKs = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS );
      final IComponent rauheitNeu;
      final IComponent rauheitAlt;
      final Object[] values;

      if( rauheitKs == null && rauheitKst == null )
      {
        rauheitNeu = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS );
        rauheitAlt = null;
        values = new Object[] { 0.0 };
      }
      else
      {
        rauheitAlt = rauheitKst == null ? rauheitKs : provider.getPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST );
        rauheitNeu = rauheitKs == null ? rauheitKst : provider.getPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS );
        values = ProfilUtil.getValuesFor( profil, rauheitAlt );
        operation.addChange( new PointPropertyRemove( profil, rauheitAlt ) );

      }
      m_targetAxisRight.setLabel( String.format( AXIS_LABEL, ComponentUtilities.getComponentUnitLabel( rauheitNeu ) ) );
      operation.addChange( new PointPropertyAdd( profil, rauheitNeu, values ) );
      new ProfilOperationJob( operation ).schedule();
    }

    if( layerId.equals( IWspmTuhhConstants.LAYER_BRUECKE ) )
    {
      final IProfilChange[] changes = new IProfilChange[1];
      changes[0] = new ProfileObjectAdd( profil, new IProfileObject[] { new BuildingBruecke( profil ) } );

      final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.4" ), profil, changes, true ); //$NON-NLS-1$
      new ProfilOperationJob( operation ).schedule();
    }
    if( layerId.equals( IWspmTuhhConstants.LAYER_WEHR ) )
    {
      final IProfilChange[] changes = new IProfilChange[1];
      final BuildingWehr bw = new BuildingWehr( profil );
      setInitialValues( bw, profil );
      changes[0] = new ProfileObjectAdd( profil, new IProfileObject[] { bw } );
      final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.5" ), profil, changes, true ); //$NON-NLS-1$
      new ProfilOperationJob( operation ).schedule();
    }
    if( layerId.equals( IWspmTuhhConstants.LAYER_TUBES ) )
    {
      final BuildingKreis building = new BuildingKreis();
      building.setValue( building.getObjectProperty( BUILDING_PROPERTY_RAUHEIT ), 0.2 );
      final IProfilChange[] changes = new IProfilChange[1];
      changes[0] = new ProfileObjectAdd( profil, new IProfileObject[] { building } );
      final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.6" ), profil, changes, true ); //$NON-NLS-1$
      new ProfilOperationJob( operation ).schedule();
    }
    if( layerId.equals( IWspmTuhhConstants.LAYER_SINUOSITAET ) )
    {
      final IProfilChange[] changes = new IProfilChange[1];

      final SinuositaetProfileObject sinObj = new SinuositaetProfileObject();
      final IRecord record = sinObj.getObservation().getResult().createRecord();
      sinObj.getObservation().getResult().add( record );

      changes[0] = new ProfileObjectAdd( profil, new IProfileObject[] { sinObj } );
      final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.4" ), profil, changes, true ); //$NON-NLS-1$
      new ProfilOperationJob( operation ).schedule();
    }
  }

  @Override
  public IProfilChartLayer createLayer( final IProfil profil, final String layerID )
  {
    if( layerID == null || profil == null )
      return null;

    final CoordinateMapper cmLeft = new CoordinateMapper( m_domainAxis, m_targetAxisLeft );
    final CoordinateMapper cmScreen = new CoordinateMapper( m_domainAxis, m_screenAxisVertical );

    if( layerID.equals( IWspmTuhhConstants.LAYER_BEWUCHS ) )
      return new VegetationTheme( profil, new IProfilChartLayer[] { new ComponentLayer( profil, IWspmConstants.POINT_PROPERTY_BEWUCHS_AX, false ),
          new ComponentLayer( profil, IWspmConstants.POINT_PROPERTY_BEWUCHS_AY, false ), new ComponentLayer( profil, IWspmConstants.POINT_PROPERTY_BEWUCHS_DP, false ) }, cmLeft, m_styleProvider );
    else if( layerID.equals( IWspmConstants.LAYER_GEOKOORDINATEN ) )
      return new GeoCoordinateTheme( profil );
    else if( layerID.equals( IWspmConstants.LAYER_COMMENT ) )
      return new CommentTheme( profil );
    else if( layerID.equals( IWspmConstants.LAYER_CODE ) )
      return new CodeTheme( profil );
    else if( layerID.equals( IWspmConstants.LAYER_GELAENDE ) )
      return new CrossSectionTheme( profil, new IProfilChartLayer[] { new StationLineLayer( profil, IWspmConstants.POINT_PROPERTY_HOEHE ),
          new StationPointLayer( layerID, profil, IWspmConstants.POINT_PROPERTY_HOEHE, m_styleProvider ) }, cmLeft );
    else if( layerID.equals( IWspmTuhhConstants.LAYER_RAUHEIT ) )
    {
      final CoordinateMapper cmRight = new CoordinateMapper( m_domainAxis, m_targetAxisRight );

      final IProfilChartLayer[] subLayers = new IProfilChartLayer[] { new RoughnessLayer( profil, IWspmConstants.POINT_PROPERTY_RAUHEIT_KST, m_styleProvider ),
          new RoughnessLayer( profil, IWspmConstants.POINT_PROPERTY_RAUHEIT_KS, m_styleProvider ) };

      return new RoughnessTheme( profil, subLayers, cmRight );
    }
    else if( layerID.equals( IWspmTuhhConstants.LAYER_BRUECKE ) )
    {
      final IProfilChartLayer[] subLayers = new IProfilChartLayer[] {
          new PointsLineLayer( layerID + "_" + IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, profil, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, m_styleProvider ), //$NON-NLS-1$
          new PointsLineLayer( layerID + "_" + IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, profil, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, m_styleProvider ) }; //$NON-NLS-1$

      return new BuildingBridgeTheme( profil, subLayers, cmLeft );
    }
    else if( layerID.equals( IWspmTuhhConstants.LAYER_WEHR ) )
    {
      final IProfilChartLayer[] subLayers = new IProfilChartLayer[] {
          new PointsLineLayer( layerID + "_" + IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, profil, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, m_styleProvider ), //$NON-NLS-1$
          new PointMarkerLayer( profil, IWspmTuhhConstants.MARKER_TYP_WEHR, m_styleProvider, 30, false ) };

      return new BuildingWeirTheme( profil, subLayers, cmLeft, cmScreen );
    }
    else if( layerID.equals( IWspmTuhhConstants.LAYER_TUBES ) )
      return new BuildingTubesTheme( profil, cmLeft, m_styleProvider );
    else if( layerID.equals( IWspmTuhhConstants.LAYER_SINUOSITAET ) )
      return new SinuositaetLayer( profil );
    else if( layerID.equals( IWspmTuhhConstants.LAYER_DEVIDER ) )
    {
      final PointMarkerLayer dbLayer = new PointMarkerLayer( profil, IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, m_styleProvider, 5, true );
      final RiverChannelLayer tfLayer = new RiverChannelLayer( profil, m_styleProvider, 15, false );
      final PointMarkerLayer bvLayer = new PointMarkerLayer( profil, IWspmTuhhConstants.MARKER_TYP_BORDVOLL, m_styleProvider, 25, false );
      final IProfilChartLayer[] subLayers = new IProfilChartLayer[] { dbLayer, tfLayer, bvLayer };

      return new DeviderTheme( profil, subLayers, cmScreen );
    }

    return null;
  }

  @Override
  public IProfilChartLayer[] createLayers( final IProfil profile, final Object result )
  {
    // Achtung: diese Reihenfolge ist die nat�rliche Ordnung im Layermanager

    final List<IProfilChartLayer> layersToAdd = new ArrayList<IProfilChartLayer>();

    /** geo coordinates */
    if( profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT ) != null )
      layersToAdd.add( createLayer( profile, IWspmConstants.LAYER_GEOKOORDINATEN ) );

    if( profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_COMMENT ) != null )
      layersToAdd.add( createLayer( profile, IWspmConstants.LAYER_COMMENT ) );
    if( profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_CODE ) != null )
      layersToAdd.add( createLayer( profile, IWspmConstants.LAYER_CODE ) );

    /** roughnesses */
    layersToAdd.add( createLayer( profile, IWspmTuhhConstants.LAYER_RAUHEIT ) );

    /** vegetation layer */
    if( profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX ) != null )
    {
      layersToAdd.add( createLayer( profile, IWspmTuhhConstants.LAYER_BEWUCHS ) );
    }

    /** profile buildings layer */
    final IProfilChartLayer buildingLayer = createBuildingLayer( profile );
    if( Objects.isNotNull( buildingLayer ) )
    {
      layersToAdd.add( buildingLayer );
    }

    /** water level layer */
    layersToAdd.add( createWspLayer( profile, (IWspmResultNode) result ) );

    /** water level fixation layer */
    layersToAdd.add( createWspFixationLayer( profile, (IWspmResultNode) result ) );

    // TODO water level fixiation level

    /** 2d layers */
    final IProfilChartLayer[] twoDLayers = create2DWaterLevelLayers( profile );
    Collections.addAll( layersToAdd, twoDLayers );

    /** ground layer */
    if( profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE ) != null )
    {
      layersToAdd.add( createLayer( profile, IWspmConstants.LAYER_GELAENDE ) );
    }

    /** profile deviders (trennfl�chen) */
    layersToAdd.add( createLayer( profile, IWspmTuhhConstants.LAYER_DEVIDER ) );

    /** sinuosit�t profile layer */
    final ISinuositaetProfileObject[] sinObj = profile.getProfileObjects( ISinuositaetProfileObject.class );
    if( ArrayUtils.isNotEmpty( sinObj ) )
    {
      layersToAdd.add( createLayer( profile, IWspmTuhhConstants.LAYER_SINUOSITAET ) );
    }

    /* Prune 'null's returned from createLayer-subroutines */
    layersToAdd.removeAll( Collections.singleton( null ) );

    // TODO why here ?
    setAxisLabel( profile );

    return layersToAdd.toArray( new IProfilChartLayer[layersToAdd.size()] );
  }

  private IProfilChartLayer[] create2DWaterLevelLayers( final IProfil profile )
  {
    final Set<IProfilChartLayer> layers = new LinkedHashSet<IProfilChartLayer>();

    final IComponent[] pointProperties = profile.getPointProperties();
    for( final IComponent property : pointProperties )
    {
      final IPhenomenon phenomenon = property.getPhenomenon();

      if( IWspmPhenomenonConstants.PHENOMENON_WATERLEVEL_2D.equals( phenomenon.getID() ) )
      {
        final PointsLineLayer layer = new PointsLineLayer( LAYER_WASSERSPIEGEL2D, profile, property.getId(), m_styleProvider );
        layer.setTitle( property.getName() );

        layer.setCoordinateMapper( new CoordinateMapper( m_domainAxis, m_targetAxisLeft ) );
        layers.add( layer );
      }
    }

    return layers.toArray( new IProfilChartLayer[] {} );

  }

  private IProfilChartLayer createBuildingLayer( final IProfil profile )
  {
    // TODO IProfileObjects now returned as list from IProfile, but we can only handle one IProfileObject (WSPM can't
    // handle more!)
    final IProfileObject[] buildings = profile.getProfileObjects( IProfileBuilding.class );
    if( Arrays.isEmpty( buildings ) )
      return null;

    for( final IProfileObject building : buildings )
    {
      if( Buildings.isBridge( building ) )
        return createLayer( profile, IWspmTuhhConstants.LAYER_BRUECKE );
      else if( Buildings.isWeir( building ) )
        return createLayer( profile, IWspmTuhhConstants.LAYER_WEHR );
      else if( Buildings.isTube( building ) )
        return createLayer( profile, LAYER_TUBES );
      else if( building.getId().equals( IWspmTuhhConstants.BUILDING_TYP_KREIS ) )
        return createLayer( profile, LAYER_TUBES );
      else if( building.getId().equals( IWspmTuhhConstants.BUILDING_TYP_EI ) )
        return createLayer( profile, LAYER_TUBES );
    }

    return null;
  }

  private IProfilChartLayer createWspLayer( final IProfil profile, final IWspmResultNode result )
  {

    final CoordinateMapper cm = new CoordinateMapper( m_domainAxis, m_targetAxisLeft );
    final IWspLayerData wspLayerData = new TuhhResultDataProvider( result, "activeIds" ); //$NON-NLS-1$

    return new WspLayer( profile, IWspmConstants.LAYER_WASSERSPIEGEL, m_styleProvider, wspLayerData, cm );
  }

  private IProfilChartLayer createWspFixationLayer( final IProfil profile, final IWspmResultNode result )
  {
    final CoordinateMapper cm = new CoordinateMapper( m_domainAxis, m_targetAxisLeft );
    final IWspLayerData data = new TuhhResultDataProvider( result, "activeFixationIds" ); //$NON-NLS-1$

    return new WspFixationLayer( profile, IWspmConstants.LAYER_WASSERSPIEGEL_FIXIERUNG, m_styleProvider, data, cm );
  }

  @Override
  public LayerDescriptor[] getAddableLayers( final ProfilChartModel chartModel )
  {
    final List<LayerDescriptor> addableLayer = new ArrayList<LayerDescriptor>();

    final List<String> existingLayers = new ArrayList<String>();

    final ILayerManager mngr = chartModel.getLayerManager();
    final IProfil profile = chartModel.getProfil();
    if( mngr == null || profile == null )
      return new LayerDescriptor[] {};

    for( final IChartLayer layer : mngr.getLayers() )
    {
      existingLayers.add( layer.getIdentifier() );
    }

    final IProfileObject[] objects = profile.getProfileObjects( IProfileBuilding.class );

    // only ONE Object allowed
    if( ArrayUtils.isEmpty( objects ) )
    {
      addableLayer.add( new LayerDescriptor( BuildingBridgeTheme.TITLE, LAYER_BRUECKE ) );
      addableLayer.add( new LayerDescriptor( BuildingWeirTheme.TITLE, IWspmTuhhConstants.LAYER_WEHR ) );
      addableLayer.add( new LayerDescriptor( BuildingTubesTheme.TITLE, IWspmTuhhConstants.LAYER_TUBES ) );
    }
    // always show devider and roughness
    if( !existingLayers.contains( IWspmTuhhConstants.LAYER_RAUHEIT ) )
    {
      addableLayer.add( new LayerDescriptor( RoughnessTheme.TITLE, IWspmTuhhConstants.LAYER_RAUHEIT ) );
    }
    if( !existingLayers.contains( IWspmTuhhConstants.LAYER_DEVIDER ) )
    {
      addableLayer.add( new LayerDescriptor( DeviderTheme.TITLE, IWspmTuhhConstants.LAYER_DEVIDER ) );
    }

    if( profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX ) == null && !existingLayers.contains( IWspmTuhhConstants.LAYER_BEWUCHS ) )
    {
      addableLayer.add( new LayerDescriptor( VegetationTheme.TITLE, IWspmTuhhConstants.LAYER_BEWUCHS ) );
    }
    if( profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE ) == null && !existingLayers.contains( IWspmConstants.LAYER_GELAENDE ) )
    {
      addableLayer.add( new LayerDescriptor( CrossSectionTheme.TITLE, IWspmConstants.LAYER_GELAENDE ) );
    }

    if( profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT ) == null && !existingLayers.contains( IWspmConstants.LAYER_GEOKOORDINATEN ) )
      addableLayer.add( new LayerDescriptor( GeoCoordinateTheme.TITLE, IWspmConstants.LAYER_GEOKOORDINATEN ) );

    if( profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_COMMENT ) == null )
      addableLayer.add( new LayerDescriptor( CommentTheme.TITLE, IWspmConstants.LAYER_COMMENT ) );

    if( profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_CODE ) == null )
      addableLayer.add( new LayerDescriptor( CodeTheme.TITLE, IWspmConstants.LAYER_CODE ) );

    final ISinuositaetProfileObject[] sinObj = profile.getProfileObjects( ISinuositaetProfileObject.class );
    if( sinObj.length < 1 && !existingLayers.contains( IWspmTuhhConstants.LAYER_SINUOSITAET ) )
    {
      addableLayer.add( new LayerDescriptor( Messages.getString( "ProfilLayerProviderTuhh.3" ), IWspmTuhhConstants.LAYER_SINUOSITAET ) ); //$NON-NLS-1$
    }

    return addableLayer.toArray( new LayerDescriptor[addableLayer.size()] );
  }

  @Override
  public IComponentUiHandlerProvider getComponentUiHandlerProvider( final IProfil profile )
  {
    return new GenericComponentUiHandlerProvider( profile );
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

  final void setAxisLabel( final IProfil profil )
  {
    final String domLabel = String.format( AXIS_LABEL, ComponentUtilities.getComponentUnitLabel( profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BREITE ) ) );
    m_domainAxis.clearLabels();
    m_domainAxis.addLabel( ChartLabelRendererFactory.getAxisLabelType( m_domainAxis.getPosition(), domLabel, new Insets( 2, 2, 2, 2 ), StyleUtils.getDefaultTextStyle() ) );
    final String leftLabel = String.format( AXIS_LABEL, ComponentUtilities.getComponentUnitLabel( profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE ) ) );
    m_targetAxisLeft.clearLabels();
    m_targetAxisLeft.addLabel( ChartLabelRendererFactory.getAxisLabelType( m_targetAxisLeft.getPosition(), leftLabel, new Insets( 2, 2, 2, 2 ), StyleUtils.getDefaultTextStyle() ) );

    final IComponent roughnessKS = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS );
    final IComponent roughnessKST = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST );
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
    m_targetAxisRight.addLabel( ChartLabelRendererFactory.getAxisLabelType( m_targetAxisRight.getPosition(), rightLabel, new Insets( 2, 2, 2, 2 ), StyleUtils.getDefaultTextStyle() ) );

  }

  private void setInitialValues( final BuildingWehr building, final IProfil profil )
  {
    final IProfilPointMarker[] marker = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    if( marker.length == 2 )
    {

      final IRecord p1 = marker[0].getPoint();
      final IRecord p2 = marker[1].getPoint();
      final int index = profil.indexOfProperty( POINT_PROPERTY_HOEHE );
      final Double y1 = ProfilUtil.getDoubleValueFor( POINT_PROPERTY_HOEHE, p1 );
      final Double y2 = ProfilUtil.getDoubleValueFor( POINT_PROPERTY_HOEHE, p2 );
      p1.setValue( index, y1 );
      p2.setValue( index, y2 );

      building.setValue( building.getObjectProperty( BUILDING_PROPERTY_FORMBEIWERT ), 1.0 );
    }
  }
}
