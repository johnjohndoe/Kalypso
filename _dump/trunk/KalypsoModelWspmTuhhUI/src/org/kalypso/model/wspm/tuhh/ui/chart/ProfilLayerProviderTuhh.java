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
import org.kalypso.model.wspm.core.profil.util.ProfilObsHelper;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.core.result.IStationResult;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingEi;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingKreis;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingMaul;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingTrapez;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;
import org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;

import de.belger.swtchart.layer.IChartLayer;

/**
 * @author kimwerner
 */
public class ProfilLayerProviderTuhh implements IProfilLayerProvider
{
  private final List<String> m_layers = new ArrayList<String>();

  public ProfilLayerProviderTuhh( )
  {
    m_layers.add( IWspmTuhhConstants.LAYER_BEWUCHS );
    m_layers.add( IWspmTuhhConstants.LAYER_GEOKOORDINATEN );
    m_layers.add( IWspmTuhhConstants.LAYER_GELAENDE );
    m_layers.add( IWspmTuhhConstants.LAYER_WASSERSPIEGEL );
    m_layers.add( IWspmTuhhConstants.LAYER_RAUHEIT_KST );
    m_layers.add( IWspmTuhhConstants.LAYER_RAUHEIT_KS );
    m_layers.add( IWspmTuhhConstants.LAYER_RAUHEIT_QUICKVIEW );
    m_layers.add( IWspmTuhhConstants.LAYER_BRUECKE );
    m_layers.add( IWspmTuhhConstants.LAYER_WEHR );
    m_layers.add( IWspmTuhhConstants.LAYER_KREIS );
    m_layers.add( IWspmTuhhConstants.LAYER_MAUL );
    m_layers.add( IWspmTuhhConstants.LAYER_TRAPEZ );
    m_layers.add( IWspmTuhhConstants.LAYER_EI );
    m_layers.add( IWspmTuhhConstants.LAYER_DEVIDER );
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider#getAddableLayers()
   */
  public String[] getAddableLayers( final ProfilChartView view )
  {
    final List<String> addableLayer = new ArrayList<String>();
    final List<String> existingLayers = new ArrayList<String>();

    for( final IChartLayer layer : view.getChart().getLayers() )
      existingLayers.add( layer.getId() );

    final IProfil profile = view.getProfil();
    if( profile.getProfileObjects().length == 0 )
    {
      addableLayer.add( IWspmTuhhConstants.LAYER_BRUECKE );
      addableLayer.add( IWspmTuhhConstants.LAYER_WEHR );
      addableLayer.add( IWspmTuhhConstants.LAYER_KREIS );
      addableLayer.add( IWspmTuhhConstants.LAYER_TRAPEZ );
      addableLayer.add( IWspmTuhhConstants.LAYER_EI );
      addableLayer.add( IWspmTuhhConstants.LAYER_MAUL );
    }
    if( !profile.hasPointProperty( ProfilObsHelper.getPropertyFromId( profile, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX ) ) && !existingLayers.contains( IWspmTuhhConstants.LAYER_BEWUCHS ) )
      addableLayer.add( IWspmTuhhConstants.LAYER_BEWUCHS );
    if( profile.hasPointProperty( ProfilObsHelper.getPropertyFromId( profile, IWspmTuhhConstants.POINT_PROPERTY_HOEHE ) ) && !existingLayers.contains( IWspmTuhhConstants.LAYER_GELAENDE ) )
      addableLayer.add( IWspmTuhhConstants.LAYER_GELAENDE );
    if( !profile.hasPointProperty( ProfilObsHelper.getPropertyFromId( profile, IWspmTuhhConstants.POINT_PROPERTY_HOCHWERT ) ) && !existingLayers.contains( IWspmTuhhConstants.LAYER_GEOKOORDINATEN ) )
      addableLayer.add( IWspmTuhhConstants.LAYER_GEOKOORDINATEN );
    if( existingLayers.contains( IWspmTuhhConstants.LAYER_RAUHEIT_QUICKVIEW ) || !existingLayers.contains( IWspmTuhhConstants.LAYER_RAUHEIT_KST ) )
      addableLayer.add( IWspmTuhhConstants.LAYER_RAUHEIT_KST );
    if( existingLayers.contains( IWspmTuhhConstants.LAYER_RAUHEIT_QUICKVIEW ) || !existingLayers.contains( IWspmTuhhConstants.LAYER_RAUHEIT_KS ) )
      addableLayer.add( IWspmTuhhConstants.LAYER_RAUHEIT_KS );
    if( !existingLayers.contains( IWspmTuhhConstants.LAYER_DEVIDER ) )
      addableLayer.add( IWspmTuhhConstants.LAYER_DEVIDER );
    if( (view.getResults().length > 0) && !existingLayers.contains( IWspmTuhhConstants.LAYER_WASSERSPIEGEL ) )
      addableLayer.add( IWspmTuhhConstants.LAYER_WASSERSPIEGEL );

    return addableLayer.toArray( new String[0] );
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider#createLayer()
   */
  public IProfilChartLayer[] addLayerToChart( final ProfilChartView view, final String layerId )
  {
    final IProfil profil = view.getProfil();
    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profil.getType() );

    if( layerId.equals( IWspmTuhhConstants.LAYER_BEWUCHS ) )
    {
      final IProfilChange[] changes = new IProfilChange[3];
      changes[0] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX ) );
      changes[1] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AY ) );
      changes[2] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_DP ) );
      final ProfilOperation operation = new ProfilOperation( "Bewuchs einfügen", view.getProfil(), changes, true );
      new ProfilOperationJob( operation ).schedule();
      return new IProfilChartLayer[] { new BewuchsLayer( view ) };
    }
    if( layerId.equals( IWspmTuhhConstants.LAYER_GEOKOORDINATEN ) )
    {
      final IProfilChange[] changes = new IProfilChange[2];
      changes[0] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT ) );
      changes[1] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT ) );
      final ProfilOperation operation = new ProfilOperation( "Geokoordinaten einfügen", view.getProfil(), changes, true );
      new ProfilOperationJob( operation ).schedule();
      return new IProfilChartLayer[] { new HochRechtsLayer( view ) };
    }
    if( layerId.equals( IWspmTuhhConstants.LAYER_GELAENDE ) )
    {
      final IProfilChange[] changes = new IProfilChange[2];
      changes[0] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE ) );
      changes[1] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmConstants.POINT_PROPERTY_BREITE ) );
      final ProfilOperation operation = new ProfilOperation( "Profillinie einfügen", view.getProfil(), changes, true );
      new ProfilOperationJob( operation ).schedule();
      return new IProfilChartLayer[] { new GelaendeLayer( view ) };
    }

    if( layerId.equals( IWspmTuhhConstants.LAYER_RAUHEIT_KS ) )
    {
      final ProfilOperation operation = new ProfilOperation( "Rauheiten einfügen", view.getProfil(), true );
      if( profil.hasPointProperty( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KST ) ) )
      {
        operation.addChange( new PointPropertyAdd( profil, provider.getPointProperty( IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KS ), ProfilUtil.getValuesFor( profil, ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KST ) ) ) );
        operation.addChange( new PointPropertyRemove( profil, provider.getPointProperty( IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KST ) ) );
      }
      else
        operation.addChange( new PointPropertyAdd( profil, provider.getPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS ) ) );
      new ProfilOperationJob( operation ).schedule();
      return new IProfilChartLayer[] { new ExtendedRauheitLayer( view, layerId, "Rauheit-ks" ) };
    }

    if( layerId.equals( IWspmTuhhConstants.LAYER_RAUHEIT_KST ) )
    {
      final ProfilOperation operation = new ProfilOperation( "Rauheiten einfügen", view.getProfil(), true );
      if( profil.hasPointProperty( ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KS ) ) )
      {
        operation.addChange( new PointPropertyAdd( profil, provider.getPointProperty( IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KST ), ProfilUtil.getValuesFor( profil, ProfilObsHelper.getPropertyFromId( profil, IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KS ) ) ) );
        operation.addChange( new PointPropertyRemove( profil, provider.getPointProperty( IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KS ) ) );
      }
      else
        operation.addChange( new PointPropertyAdd( profil, provider.getPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST ) ) );
      new ProfilOperationJob( operation ).schedule();
      return new IProfilChartLayer[] { new ExtendedRauheitLayer( view, layerId, "Rauheit-kst" ) };
    }
    if( layerId.equals( IWspmTuhhConstants.LAYER_BRUECKE ) )
    {
      final IProfilChange[] changes = new IProfilChange[1];
      changes[0] = new ProfileObjectSet( profil, new IProfileObject[] { new BuildingBruecke( profil ) } );

      final ProfilOperation operation = new ProfilOperation( "Brücke einfügen", view.getProfil(), changes, true );
      new ProfilOperationJob( operation ).schedule();
      return new IProfilChartLayer[] { new BrueckeBuildingLayer( view ) };
    }
    if( layerId.equals( IWspmTuhhConstants.LAYER_WEHR ) )
    {
      final IProfilChange[] changes = new IProfilChange[1];
      changes[0] = new ProfileObjectSet( profil, new IProfileObject[] { new BuildingWehr( profil ) } );

      final ProfilOperation operation = new ProfilOperation( "Wehr einfügen", view.getProfil(), changes, true );
      new ProfilOperationJob( operation ).schedule();
      return new IProfilChartLayer[] { new WehrBuildingLayer( view ) };
    }
    if( layerId.equals( IWspmTuhhConstants.LAYER_KREIS ) )
    {
      final IProfileObject building = new BuildingKreis( profil );
      final IProfilChange[] changes = new IProfilChange[1];
      changes[0] = new ProfileObjectSet( profil, new IProfileObject[] { building } );
      final ProfilOperation operation = new ProfilOperation( "Durchlaß einfügen", view.getProfil(), changes, true );
      new ProfilOperationJob( operation ).schedule();
      return new IProfilChartLayer[] { new KreisBuildingLayer( view ) };
    }
    if( layerId.equals( IWspmTuhhConstants.LAYER_MAUL ) )
    {
      final IProfileObject building = new BuildingMaul( profil );
      final IProfilChange[] changes = new IProfilChange[1];
      changes[0] = new ProfileObjectSet( profil, new IProfileObject[] { building } );
      final ProfilOperation operation = new ProfilOperation( "Durchlaß einfügen", view.getProfil(), changes, true );
      new ProfilOperationJob( operation ).schedule();
      return new IProfilChartLayer[] { new MaulBuildingLayer( view ) };
    }
    if( layerId.equals( IWspmTuhhConstants.LAYER_TRAPEZ ) )
    {
      final IProfileObject building = new BuildingTrapez( profil );
      final IProfilChange[] changes = new IProfilChange[1];
      changes[0] = new ProfileObjectSet( profil, new IProfileObject[] { building } );
      final ProfilOperation operation = new ProfilOperation( "Durchlaß einfügen", view.getProfil(), changes, true );
      new ProfilOperationJob( operation ).schedule();
      return new IProfilChartLayer[] { new TrapezBuildingLayer( view ) };
    }
    if( layerId.equals( IWspmTuhhConstants.LAYER_EI ) )
    {
      final IProfileObject building = new BuildingEi( profil );
      final IProfilChange[] changes = new IProfilChange[1];
      changes[0] = new ProfileObjectSet( profil, new IProfileObject[] { building } );
      final ProfilOperation operation = new ProfilOperation( "Durchlaß einfügen", view.getProfil(), changes, true );
      new ProfilOperationJob( operation ).schedule();
      return new IProfilChartLayer[] { new EiBuildingLayer( view ) };
    }

    return getLayer( layerId, view );
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider#providesLayer(java.lang.String)
   */
  public boolean providesLayer( final String layerId )
  {
    return m_layers.contains( layerId );
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider#addRequieredLayer(org.kalypso.model.wspm.ui.view.chart.ProfilChartView)
   */
  public String[] getRequiredLayer( final ProfilChartView view )
  {
    final ArrayList<String> layerToAdd = new ArrayList<String>();
    final IProfil profile = view.getProfil();
    if( profile.hasPointProperty( ProfilObsHelper.getPropertyFromId( profile, IWspmTuhhConstants.POINT_PROPERTY_HOEHE ) ) )
      layerToAdd.add( IWspmTuhhConstants.LAYER_GELAENDE );

    if( profile.hasPointProperty( ProfilObsHelper.getPropertyFromId( profile, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX ) ) )
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
      else if( building.getId().equals( IWspmTuhhConstants.BUILDING_TYP_KREIS ) )
        layerToAdd.add( IWspmTuhhConstants.LAYER_KREIS );
      else if( building.getId().equals( IWspmTuhhConstants.BUILDING_TYP_TRAPEZ ) )
        layerToAdd.add( IWspmTuhhConstants.LAYER_TRAPEZ );
      else if( building.getId().equals( IWspmTuhhConstants.BUILDING_TYP_EI ) )
        layerToAdd.add( IWspmTuhhConstants.LAYER_EI );
      else if( building.getId().equals( IWspmTuhhConstants.BUILDING_TYP_MAUL ) )
        layerToAdd.add( IWspmTuhhConstants.LAYER_MAUL );
    }

    /* We always have a trenner layer, even if no trenner is defined. */
    layerToAdd.add( IWspmTuhhConstants.LAYER_DEVIDER );

    if( profile.hasPointProperty( ProfilObsHelper.getPropertyFromId( profile, IWspmTuhhConstants.POINT_PROPERTY_HOCHWERT ) ) )
      layerToAdd.add( IWspmTuhhConstants.LAYER_GEOKOORDINATEN );
    if( view.getResults().length > 0 )
      layerToAdd.add( IWspmTuhhConstants.LAYER_WASSERSPIEGEL );

    if( profile.hasPointProperty( ProfilObsHelper.getPropertyFromId( profile, IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KST ) ) )
      layerToAdd.add( IWspmTuhhConstants.LAYER_RAUHEIT_KST );
    if( profile.hasPointProperty( ProfilObsHelper.getPropertyFromId( profile, IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KS ) ) )
      layerToAdd.add( IWspmTuhhConstants.LAYER_RAUHEIT_KS );

    return layerToAdd.toArray( new String[0] );
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilLayerProvider#getLayer(java.lang.String)
   */
  public IProfilChartLayer[] getLayer( final String layerId, final ProfilChartView view )
  {
    if( layerId.equals( IWspmTuhhConstants.LAYER_BEWUCHS ) )
      return new IProfilChartLayer[] { new BewuchsLayer( view ) };
    if( layerId.equals( IWspmTuhhConstants.LAYER_GEOKOORDINATEN ) )
      return new IProfilChartLayer[] { new HochRechtsLayer( view ) };
    if( layerId.equals( IWspmTuhhConstants.LAYER_GELAENDE ) )
      return new IProfilChartLayer[] { new GelaendeLayer( view ) };

    if( layerId.equals( IWspmTuhhConstants.LAYER_RAUHEIT_KS ) )
      return new IProfilChartLayer[] { new ExtendedRauheitLayer( view, layerId, "Rauheit-ks" ) };
    if( layerId.equals( IWspmTuhhConstants.LAYER_RAUHEIT_KST ) )
      return new IProfilChartLayer[] { new ExtendedRauheitLayer( view, layerId, "Rauheit-kst" ) };
    if( layerId.equals( IWspmTuhhConstants.LAYER_RAUHEIT_QUICKVIEW ) )
      return new IProfilChartLayer[] { new SimpleRauheitLayer( view, layerId, "Rauheit" ) };
    if( layerId.equals( IWspmTuhhConstants.LAYER_BRUECKE ) )
      return new IProfilChartLayer[] { new BrueckeBuildingLayer( view ) };
    if( layerId.equals( IWspmTuhhConstants.LAYER_WEHR ) )
      return new IProfilChartLayer[] { new WehrBuildingLayer( view ) };
    if( layerId.equals( IWspmTuhhConstants.LAYER_KREIS ) )
      return new IProfilChartLayer[] { new KreisBuildingLayer( view ) };
    if( layerId.equals( IWspmTuhhConstants.LAYER_MAUL ) )
      return new IProfilChartLayer[] { new MaulBuildingLayer( view ) };
    if( layerId.equals( IWspmTuhhConstants.LAYER_TRAPEZ ) )
      return new IProfilChartLayer[] { new TrapezBuildingLayer( view ) };
    if( layerId.equals( IWspmTuhhConstants.LAYER_EI ) )
      return new IProfilChartLayer[] { new EiBuildingLayer( view ) };
    if( layerId.equals( IWspmTuhhConstants.LAYER_DEVIDER ) )
      return new IProfilChartLayer[] { new TrennerLayer( view ) };
    if( layerId.equals( IWspmTuhhConstants.LAYER_WASSERSPIEGEL ) )
    {
      final List<IStationResult> resultLayers = new ArrayList<IStationResult>();
      for( final IStationResult result : view.getResults() )
      {
        resultLayers.add( new WspLayer( view, result ) );
      }
      return resultLayers.toArray( new IProfilChartLayer[0] );
    }
    return new IProfilChartLayer[0];
  }

}
