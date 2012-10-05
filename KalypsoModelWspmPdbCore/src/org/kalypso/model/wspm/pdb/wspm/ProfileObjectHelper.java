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
package org.kalypso.model.wspm.pdb.wspm;

import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfileMetadata;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecord;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecords;
import org.kalypso.model.wspm.core.profil.util.ProfileUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.energyloss.EnergylossProfileObject;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.GenericProfileHorizon;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingEi;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingKreis;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingMaul;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingTrapez;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.SinuositaetProfileObject;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * @author Holger Albert
 */
public class ProfileObjectHelper
{
  private ProfileObjectHelper( )
  {
  }

  /**
   * This function creates a new profile object. It uses the class of the given profile object.
   * 
   * @param profileObject
   *          The class of this object will be used for the new profile object.
   * @return A new profile object or null.
   */
  public static IProfileObject createProfileObject( final IProfileObject profileObject )
  {
    if( profileObject instanceof BuildingBruecke )
      return new BuildingBruecke( null );

    if( profileObject instanceof BuildingWehr )
      return new BuildingWehr( null );

    if( profileObject instanceof BuildingEi )
      return new BuildingEi();

    if( profileObject instanceof BuildingKreis )
      return new BuildingKreis();

    if( profileObject instanceof BuildingMaul )
      return new BuildingMaul();

    if( profileObject instanceof BuildingTrapez )
      return new BuildingTrapez();

    if( profileObject instanceof SinuositaetProfileObject )
      return new SinuositaetProfileObject();

    if( profileObject instanceof EnergylossProfileObject )
      return new EnergylossProfileObject();

    return null;
  }

  public static void cloneProfileObject( final IProfileObject source, final IProfileObject target )
  {
    /* Clone the description. */
    final String description = source.getDescription();
    target.setDescription( description );

    /* Clone the metadata. */
    final IProfileMetadata sourceMetadata = source.getMetadata();
    final IProfileMetadata targetMetadata = target.getMetadata();
    final String[] keys = sourceMetadata.getKeys();
    for( final String key : keys )
    {
      final String value = sourceMetadata.getMetadata( key );
      targetMetadata.setMetadata( key, value );
    }

    /* Clone the records. */
    final IProfileObjectRecords sourceRecords = source.getRecords();
    final IProfileObjectRecords targetRecords = target.getRecords();
    for( int i = 0; i < sourceRecords.getSize(); i++ )
    {
      final IProfileObjectRecord sourceRecord = sourceRecords.getRecord( i );

      final IProfileObjectRecord targetRecord = targetRecords.addNewRecord();
      targetRecord.setId( sourceRecord.getId() );
      targetRecord.setComment( sourceRecord.getComment() );
      targetRecord.setBreite( sourceRecord.getBreite() );
      targetRecord.setHoehe( sourceRecord.getHoehe() );
      targetRecord.setRechtswert( sourceRecord.getRechtswert() );
      targetRecord.setHochwert( sourceRecord.getHochwert() );
      targetRecord.setCode( sourceRecord.getCode() );
    }
  }

  public static void updateBridgeFromComponents( final IProfile source, final BuildingBruecke ukTarget, final GenericProfileHorizon okTarget )
  {
    final TupleResult result = source.getResult();
    final IComponent ukBrueckeComponent = ProfileUtil.getFeatureComponent( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE );
    final IComponent okBrueckeComponent = ProfileUtil.getFeatureComponent( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE );
    final int ukBrueckeIndex = result.indexOfComponent( ukBrueckeComponent );
    final int okBrueckeIndex = result.indexOfComponent( okBrueckeComponent );

    final IProfileObjectRecords ukRecords = ukTarget.getRecords();
    final IProfileObjectRecords okRecords = okTarget.getRecords();

    for( final IRecord record : result )
    {
      /* Only records with a height in the special component may be copied. */
      final Double ukHeight = (Double)record.getValue( ukBrueckeIndex );
      if( ukHeight == null )
      {
        /* Add a new record to the profile object. */
        final IProfileObjectRecord ukRecord = ukRecords.addNewRecord();

        /* Copy all standard values from the profile record to the profile object record. */
        org.kalypso.model.wspm.core.profil.ProfileObjectHelper.updateStandardProperties( record, ukRecord );

        /* Replace the height with the height from the special component. */
        ukRecord.setHoehe( ukHeight );
      }

      /* Only records with a height in the special component may be copied. */
      final Double okHeight = (Double)record.getValue( okBrueckeIndex );
      if( okHeight == null )
      {
        /* Add a new record to the profile object. */
        final IProfileObjectRecord okRecord = okRecords.addNewRecord();

        /* Copy all standard values from the profile record to the profile object record. */
        org.kalypso.model.wspm.core.profil.ProfileObjectHelper.updateStandardProperties( record, okRecord );

        /* Replace the height with the height from the special component. */
        okRecord.setHoehe( okHeight );
      }
    }
  }

  public static void updateWeirFromComponents( final IProfile source, final BuildingWehr okTarget )
  {
    final TupleResult result = source.getResult();
    final IComponent okWehrComponent = ProfileUtil.getFeatureComponent( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR );
    final int okWehrIndex = result.indexOfComponent( okWehrComponent );

    final IProfileObjectRecords okRecords = okTarget.getRecords();

    for( final IRecord record : result )
    {
      /* Only records with a height in the special component may be copied. */
      final Double height = (Double)record.getValue( okWehrIndex );
      if( height == null )
        continue;

      /* Add a new record to the profile object. */
      final IProfileObjectRecord okRecord = okRecords.addNewRecord();

      /* Copy all standard values from the profile record to the profile object record. */
      org.kalypso.model.wspm.core.profil.ProfileObjectHelper.updateStandardProperties( record, okRecord );

      /* Replace the height with the height from the special component. */
      okRecord.setHoehe( height );
    }
  }

  public static void updateEggFromComponents( final IProfile profile, final BuildingEi clonedProfileObject )
  {
    // TODO
  }

  public static void updateCircleFromComponents( final IProfile profile, final BuildingKreis clonedProfileObject )
  {
    // TODO
  }
}