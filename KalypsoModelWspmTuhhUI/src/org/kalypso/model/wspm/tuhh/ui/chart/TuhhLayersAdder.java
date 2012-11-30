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

import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.commons.java.lang.Arrays;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfileChange;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.core.profil.IProfilePointPropertyProvider;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyAdd;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.changes.ProfileObjectAdd;
import org.kalypso.model.wspm.core.profil.changes.ProfileObjectRemove;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperation;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperationJob;
import org.kalypso.model.wspm.core.profil.util.ProfileUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.energyloss.EnergylossProfileObject;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingKreis;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.IProfileBuilding;
import org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.SinuositaetProfileObject;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

import de.openali.odysseus.chart.framework.model.mapper.IAxis;

/**
 * FIXME: ugly! introduce class 'LayerAdder' and have implementations for each type
 *
 * @author Dirk Kuch
 */
public final class TuhhLayersAdder
{
  private TuhhLayersAdder( )
  {
  }

  public static void addVegetationLayer( final IProfilePointPropertyProvider provider, final IProfile profil )
  {
    final IProfileChange[] changes = new IProfileChange[3];
    changes[0] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AX ), 0.0 );
    changes[1] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AY ), 0.0 );
    changes[2] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_DP ), 0.0 );

    final ProfileOperation operation = new ProfileOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.0" ), profil, changes, true ); //$NON-NLS-1$
    new ProfileOperationJob( operation ).schedule();
  }

  public static void addGeoLayer( final IProfilePointPropertyProvider provider, final IProfile profil )
  {
    final IProfileChange[] changes = new IProfileChange[2];
    changes[0] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmPointProperties.POINT_PROPERTY_HOCHWERT ) );
    changes[1] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmPointProperties.POINT_PROPERTY_RECHTSWERT ) );

    final ProfileOperation operation = new ProfileOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.1" ), profil, changes, true ); //$NON-NLS-1$
    new ProfileOperationJob( operation ).schedule();
  }

  public static void addGroundLayer( final IProfilePointPropertyProvider provider, final IProfile profil )
  {
    final IProfileChange[] changes = new IProfileChange[2];
    changes[0] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmPointProperties.POINT_PROPERTY_HOEHE ) );
    changes[1] = new PointPropertyAdd( profil, provider.getPointProperty( IWspmPointProperties.POINT_PROPERTY_BREITE ) );

    final ProfileOperation operation = new ProfileOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.2" ), profil, changes, true ); //$NON-NLS-1$
    new ProfileOperationJob( operation ).schedule();
  }

  public static void addRoughnessLayers( final IProfilePointPropertyProvider provider, final IProfile profil, final IAxis targetAxisRight, final String axisLabeling )
  {
    final ProfileOperation operation = new ProfileOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.3" ), profil, true ); //$NON-NLS-1$
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
      values = ProfileUtil.getValuesFor( profil, rauheitAlt );
      operation.addChange( new PointPropertyRemove( profil, rauheitAlt ) );
    }

    targetAxisRight.setLabel( String.format( axisLabeling, ComponentUtilities.getComponentUnitLabel( rauheitNeu ) ) );
    operation.addChange( new PointPropertyAdd( profil, rauheitNeu, values ) );
    new ProfileOperationJob( operation ).schedule();
  }

  public static void addBridgeLayer( final Shell shell, final IProfile profil )
  {
    final IProfileBuilding[] existingBuildings = profil.getProfileObjects( IProfileBuilding.class );
    if( !askForExistingBuildings( shell, existingBuildings ) )
      return;

    final ProfileObjectRemove removeChange = new ProfileObjectRemove( profil, existingBuildings );
    final ProfileObjectAdd addChange = new ProfileObjectAdd( profil, new BuildingBruecke( profil ) );

    final IProfileChange[] changes = new IProfileChange[] { removeChange, addChange };

    final ProfileOperation operation = new ProfileOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.4" ), profil, changes, true ); //$NON-NLS-1$
    new ProfileOperationJob( operation ).schedule();
  }

  private static boolean askForExistingBuildings( final Shell shell, final IProfileBuilding[] existingBuildings )
  {
    if( Arrays.isEmpty( existingBuildings ) )
      return true;

    return MessageDialog.openConfirm( shell, Messages.getString("TuhhLayersAdder.0"), Messages.getString("TuhhLayersAdder.1") ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public static void addWeirLayer( final Shell shell, final IProfile profil )
  {
    final IProfileBuilding[] existingBuildings = profil.getProfileObjects( IProfileBuilding.class );
    if( !askForExistingBuildings( shell, existingBuildings ) )
      return;

    final BuildingWehr bw = new BuildingWehr( profil );

    setInitialWeirValues( bw, profil );

    // REMARK: Does not delete the ok parts for bridges (in every add method)...
    final ProfileObjectRemove removeChange = new ProfileObjectRemove( profil, existingBuildings );
    final ProfileObjectAdd addChange = new ProfileObjectAdd( profil, bw );
    final IProfileChange[] changes = new IProfileChange[] { removeChange, addChange };

    final ProfileOperation operation = new ProfileOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.5" ), profil, changes, true ); //$NON-NLS-1$
    new ProfileOperationJob( operation ).schedule();
  }

  private static void setInitialWeirValues( final BuildingWehr building, final IProfile profil )
  {
    final IProfilePointMarker[] marker = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    if( marker.length == 2 )
    {
      final IRecord p1 = marker[0].getPoint();
      final IRecord p2 = marker[1].getPoint();
      final int index = profil.indexOfProperty( IWspmPointProperties.POINT_PROPERTY_HOEHE );
      final Double y1 = ProfileUtil.getDoubleValueFor( IWspmPointProperties.POINT_PROPERTY_HOEHE, p1 );
      final Double y2 = ProfileUtil.getDoubleValueFor( IWspmPointProperties.POINT_PROPERTY_HOEHE, p2 );
      p1.setValue( index, y1 );
      p2.setValue( index, y2 );

      building.setFormbeiwert( 1.0 );
    }
  }

  public static void addTubesLayer( final Shell shell, final IProfile profil )
  {
    final IProfileBuilding[] existingBuildings = profil.getProfileObjects( IProfileBuilding.class );
    if( !askForExistingBuildings( shell, existingBuildings ) )
      return;

    final ProfileObjectRemove removeChange = new ProfileObjectRemove( profil, existingBuildings );

    final BuildingKreis building = new BuildingKreis();
    building.setRauheit( 0.2 );

    final ProfileObjectAdd addChange = new ProfileObjectAdd( profil, building );

    final IProfileChange[] changes = new IProfileChange[] { removeChange, addChange };

    final ProfileOperation operation = new ProfileOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.6" ), profil, changes, true ); //$NON-NLS-1$
    new ProfileOperationJob( operation ).schedule();
  }

  public static void addSinuositaetLayer( final IProfile profil )
  {
    final SinuositaetProfileObject sinObj = new SinuositaetProfileObject();

    final IProfileObject[] existingObjects = profil.getProfileObjects( SinuositaetProfileObject.class );

    final ProfileObjectRemove removeChange = new ProfileObjectRemove( profil, existingObjects );

    final ProfileObjectAdd addChange = new ProfileObjectAdd( profil, sinObj );

    final IProfileChange[] changes = new IProfileChange[] { removeChange, addChange };

    final ProfileOperation operation = new ProfileOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.4" ), profil, changes, true ); //$NON-NLS-1$
    new ProfileOperationJob( operation ).schedule();
  }

  public static void addEnergylossLayer( final IProfile profil )
  {
    final EnergylossProfileObject elpoObj = new EnergylossProfileObject();

    final IProfileObject[] existingObjects = profil.getProfileObjects( EnergylossProfileObject.class );

    final ProfileObjectRemove removeChange = new ProfileObjectRemove( profil, existingObjects );

    final ProfileObjectAdd addChange = new ProfileObjectAdd( profil, elpoObj );

    final IProfileChange[] changes = new IProfileChange[] { removeChange, addChange };

    final ProfileOperation operation = new ProfileOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.ProfilLayerProviderTuhh.8" ), profil, changes, true ); //$NON-NLS-1$
    new ProfileOperationJob( operation ).schedule();
  }

  public static void addCodeLayer( final IProfilePointPropertyProvider provider, final IProfile profil )
  {
    final IProfileChange change = new PointPropertyAdd( profil, provider.getPointProperty( IWspmPointProperties.POINT_PROPERTY_CODE ), StringUtils.EMPTY );

    final ProfileOperation operation = new ProfileOperation( "Insert Code", profil, change, true ); //$NON-NLS-1$
    new ProfileOperationJob( operation ).schedule();
  }

  public static void addCommentLayer( final IProfilePointPropertyProvider provider, final IProfile profil )
  {
    final IProfileChange change = new PointPropertyAdd( profil, provider.getPointProperty( IWspmPointProperties.POINT_PROPERTY_COMMENT ), StringUtils.EMPTY );

    final ProfileOperation operation = new ProfileOperation( "Insert Comments", profil, change, true ); //$NON-NLS-1$
    new ProfileOperationJob( operation ).schedule();
  }

  public static void addSeccondProfileLayer( final IProfilePointPropertyProvider provider, final IProfile profil )
  {
    SecondProfileDataManager.instance().addData( profil );
    // TODO: fire event, so chart is re-created
  }
}