/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building;

import java.util.Set;

import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.gml.classifications.IRoughnessClass;
import org.kalypso.model.wspm.core.gml.classifications.IVegetationClass;
import org.kalypso.model.wspm.core.gml.classifications.IWspmClassification;
import org.kalypso.model.wspm.core.gml.classifications.helper.WspmClassifications;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecord;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecords;
import org.kalypso.model.wspm.core.profil.IProfilePointPropertyProvider;
import org.kalypso.model.wspm.core.profil.visitors.ProfileVisitors;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.core.profil.wrappers.Profiles;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * @author Holger Albert
 */
public class BuildingUtilities
{
  public BuildingUtilities( )
  {
  }

  public static void repairBridges( final IProfileObject[] profileObjects )
  {
    /* The used ids. */
    final Set<String> usedIds = BridgeIdHelper.findUsedIds( profileObjects );

    /* Check each profile object. */
    for( final IProfileObject profileObject : profileObjects )
    {
      /* Repair bridges. */
      if( profileObject instanceof BuildingBruecke )
      {
        /* Cast. */
        final BuildingBruecke bridge = (BuildingBruecke)profileObject;

        /* If a ok profile object was found, both, bridge and ok profile object have an id. */
        final IProfileObject okProfileObject = BuildingBruecke.findOkProfileObject( bridge, profileObjects );
        if( okProfileObject != null )
          continue;

        /* We create/reuse the bridge id. */
        final String bridgeId = BridgeIdHelper.findFreeId( bridge, usedIds );
        bridge.setBrueckeId( bridgeId );

        /* Search for ok profile object without bridge id and use it. */
        final IProfileObject okPO = BridgeIdHelper.findOkProfileObjectWithoutId( profileObjects );
        if( okPO != null )
        {
          okPO.setValue( BuildingBruecke.KEY_BRUECKE_ID, bridgeId );
          continue;
        }

        /* REMARK: We do not create new ok profile objects. */
      }
    }
  }

  public static void updateComponents( final IProfileObject profileObject, final IProfile profile, final IProfilePointPropertyProvider provider )
  {
    if( profileObject instanceof BuildingBruecke )
    {
      final BuildingBruecke bridge = (BuildingBruecke)profileObject;
      insertRecordsAs( bridge, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, profile, provider );

      final IProfileObject ok = bridge.findOkProfileObject( profile );
      if( ok != null )
        insertRecordsAs( ok, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, profile, provider );
    }

    if( profileObject instanceof BuildingWehr )
    {
      final BuildingWehr weir = (BuildingWehr)profileObject;
      insertRecordsAs( weir, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR, profile, provider );
    }
  }

  private static void insertRecordsAs( final IProfileObject profileObject, final String asComponent, final IProfile profile, final IProfilePointPropertyProvider provider )
  {
    final IWspmClassification classification = WspmClassifications.getClassification( profile );
    final IVegetationClass unknownVegetationClass = classification.findUnknownVegetationClass();
    final IRoughnessClass unknownRoughnessClass = classification.findUnknownRoughnessClass();

    final String unknownVegetation = unknownVegetationClass == null ? null : unknownVegetationClass.getName();
    final String unknownRoughness = unknownRoughnessClass == null ? null : unknownRoughnessClass.getName();

    final IProfileObjectRecords records = profileObject.getRecords();
    for( int i = 0; i < records.size(); i++ )
    {
      final IProfileObjectRecord record = records.getRecord( i );
      final String id = record.getId();
      final String comment = record.getComment();
      final Double breite = record.getBreite();
      final Double hoehe = record.getHoehe();
      final Double rechtswert = record.getRechtswert();
      final Double hochwert = record.getHochwert();
      final String code = record.getCode();

      /* Find or insert point at 'width'. */
      final boolean insert = Objects.isNull( ProfileVisitors.findPoint( profile, breite.doubleValue() ) );
      final IProfileRecord pRecord = Profiles.addOrFindPoint( profile, breite );

      setValue( pRecord, asComponent, hoehe, provider );

      // TODO: Check: If we have the same width, but different rw/hw, we just forget the old rw/hw here, which is bad...
      // TODO: Same holds for ID, Code, etc.
      if( insert )
      {
        setValue( pRecord, IWspmPointProperties.POINT_PROPERTY_ID, id, provider );
        setValue( pRecord, IWspmPointProperties.POINT_PROPERTY_COMMENT, comment, provider );
        setValue( pRecord, IWspmPointProperties.POINT_PROPERTY_RECHTSWERT, rechtswert, provider );
        setValue( pRecord, IWspmPointProperties.POINT_PROPERTY_HOCHWERT, hochwert, provider );
        setValue( pRecord, IWspmPointProperties.POINT_PROPERTY_CODE, code, provider );

        // HOTFIX: if a new point is inserted we need a roughness/vegetation class. Unfortunately, the class values
        // are not stored in the corresponding profile object...
        // For now, we just set the 'unknown' classes here, assuming, that the buildings never have associated roughness values
        // TODO: either remember the real clases from the profile part; or a t least, use the class of the previous point.
        setValue( pRecord, IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_CLASS, unknownRoughness, provider );
        setValue( pRecord, IWspmPointProperties.POINT_PROPERTY_BEWUCHS_CLASS, unknownVegetation, provider );
      }
    }
  }

  public static void setValue( final IRecord record, final String componentID, final Object value, final IProfilePointPropertyProvider provider )
  {
    if( value == null )
      return;

    final int component = ensureComponent( record, componentID, provider );
    record.setValue( component, value );
  }

  public static int ensureComponent( final IRecord record, final String componentID, final IProfilePointPropertyProvider provider )
  {
    final TupleResult owner = record.getOwner();
    final int index = owner.indexOfComponent( componentID );
    if( index != -1 )
      return index;

    owner.addComponent( provider.getPointProperty( componentID ) );
    return owner.indexOfComponent( componentID );
  }
}