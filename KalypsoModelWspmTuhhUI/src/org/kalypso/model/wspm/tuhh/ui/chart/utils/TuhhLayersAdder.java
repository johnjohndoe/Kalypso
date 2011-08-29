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

import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyAdd;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.changes.ProfileObjectAdd;
import org.kalypso.model.wspm.core.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.core.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingKreis;
import org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.SinuositaetProfileObject;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

import de.openali.odysseus.chart.framework.model.mapper.IAxis;

/**
 * @author Dirk Kuch
 */
public final class TuhhLayersAdder
{
  private TuhhLayersAdder( )
  {
  }

  public static void addVegetationLayer( final IProfilPointPropertyProvider provider, final IProfil profil )
  {
    final IProfilChange[] changes = new IProfilChange[3];
    changes[0] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AX ), 0.0 );
    changes[1] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AY ), 0.0 );
    changes[2] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_DP ), 0.0 );

    final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.0" ), profil, changes, true ); //$NON-NLS-1$
    new ProfilOperationJob( operation ).schedule();
  }

  public static void addGeoLayer( final IProfilPointPropertyProvider provider, final IProfil profil )
  {
    final IProfilChange[] changes = new IProfilChange[2];
    changes[0] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmPointProperties.POINT_PROPERTY_HOCHWERT ) );
    changes[1] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmPointProperties.POINT_PROPERTY_RECHTSWERT ) );

    final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.1" ), profil, changes, true ); //$NON-NLS-1$
    new ProfilOperationJob( operation ).schedule();
  }

  public static void addGroundLayer( final IProfilPointPropertyProvider provider, final IProfil profil )
  {
    final IProfilChange[] changes = new IProfilChange[2];
    changes[0] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmPointProperties.POINT_PROPERTY_HOEHE ) );
    changes[1] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmPointProperties.POINT_PROPERTY_BREITE ) );

    final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.2" ), profil, changes, true ); //$NON-NLS-1$
    new ProfilOperationJob( operation ).schedule();
  }

  public static void addRoughnessLayers( final IProfilPointPropertyProvider provider, final IProfil profil, final IAxis targetAxisRight, final String axisLabeling )
  {
    final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.3" ), profil, true ); //$NON-NLS-1$
    final IComponent rauheitKst = profil.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST );
    final IComponent rauheitKs = profil.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS );
    final IComponent rauheitNeu;
    final IComponent rauheitAlt;
    final Object[] values;

    if( rauheitKs == null && rauheitKst == null )
    {
      rauheitNeu = provider.getPointProperty( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS );
      rauheitAlt = null;
      values = new Object[] { 0.0 };
    }
    else
    {
      rauheitAlt = rauheitKst == null ? rauheitKs : provider.getPointProperty( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST );
      rauheitNeu = rauheitKs == null ? rauheitKst : provider.getPointProperty( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS );
      values = ProfilUtil.getValuesFor( profil, rauheitAlt );
      operation.addChange( new PointPropertyRemove( profil, rauheitAlt ) );

    }

    targetAxisRight.setLabel( String.format( axisLabeling, ComponentUtilities.getComponentUnitLabel( rauheitNeu ) ) );
    operation.addChange( new PointPropertyAdd( profil, rauheitNeu, values ) );
    new ProfilOperationJob( operation ).schedule();
  }

  public static void addBridgeLayer( final IProfil profil )
  {
    final IProfilChange[] changes = new IProfilChange[1];
    changes[0] = new ProfileObjectAdd( profil, new IProfileObject[] { new BuildingBruecke( profil ) } );

    final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.4" ), profil, changes, true ); //$NON-NLS-1$
    new ProfilOperationJob( operation ).schedule();
  }

  public static void addWeirLayer( final IProfil profil )
  {
    final IProfilChange[] changes = new IProfilChange[1];
    final BuildingWehr bw = new BuildingWehr( profil );

    setInitialWeirValues( bw, profil );
    changes[0] = new ProfileObjectAdd( profil, new IProfileObject[] { bw } );

    final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.5" ), profil, changes, true ); //$NON-NLS-1$
    new ProfilOperationJob( operation ).schedule();
  }

  private static void setInitialWeirValues( final BuildingWehr building, final IProfil profil )
  {
    final IProfilPointMarker[] marker = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    if( marker.length == 2 )
    {

      final IRecord p1 = marker[0].getPoint();
      final IRecord p2 = marker[1].getPoint();
      final int index = profil.indexOfProperty( IWspmPointProperties.POINT_PROPERTY_HOEHE );
      final Double y1 = ProfilUtil.getDoubleValueFor( IWspmPointProperties.POINT_PROPERTY_HOEHE, p1 );
      final Double y2 = ProfilUtil.getDoubleValueFor( IWspmPointProperties.POINT_PROPERTY_HOEHE, p2 );
      p1.setValue( index, y1 );
      p2.setValue( index, y2 );

      building.setValue( building.getObjectProperty( IWspmTuhhConstants.BUILDING_PROPERTY_FORMBEIWERT ), 1.0 );
    }
  }

  public static void addTubesLayer( final IProfil profil )
  {
    final BuildingKreis building = new BuildingKreis();
    building.setValue( building.getObjectProperty( IWspmTuhhConstants.BUILDING_PROPERTY_RAUHEIT ), 0.2 );
    final IProfilChange[] changes = new IProfilChange[1];
    changes[0] = new ProfileObjectAdd( profil, new IProfileObject[] { building } );

    final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.6" ), profil, changes, true ); //$NON-NLS-1$
    new ProfilOperationJob( operation ).schedule();

  }

  public static void addSinuositaetLayer( final IProfil profil )
  {
    final IProfilChange[] changes = new IProfilChange[1];

    final SinuositaetProfileObject sinObj = new SinuositaetProfileObject();
    final IRecord record = sinObj.getObservation().getResult().createRecord();
    sinObj.getObservation().getResult().add( record );

    changes[0] = new ProfileObjectAdd( profil, new IProfileObject[] { sinObj } );
    final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.4" ), profil, changes, true ); //$NON-NLS-1$
    new ProfilOperationJob( operation ).schedule();

  }

  public static void addCodeLayer( final IProfilPointPropertyProvider provider, final IProfil profil )
  {
    final IProfilChange change = new PointPropertyAdd( profil, provider.getPointProperty( IWspmPointProperties.POINT_PROPERTY_CODE ), "" );

    final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.0" ), profil, new IProfilChange[] { change }, true ); //$NON-NLS-1$
    new ProfilOperationJob( operation ).schedule();

  }
}
