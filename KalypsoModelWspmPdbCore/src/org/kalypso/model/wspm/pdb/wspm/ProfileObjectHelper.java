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

import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfileMetadata;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecord;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecords;
import org.kalypso.model.wspm.core.profil.ProfileObjectFactory;
import org.kalypso.model.wspm.core.profil.util.ProfileUtil;
import org.kalypso.model.wspm.core.util.WspmProfileHelper;
import org.kalypso.model.wspm.pdb.gaf.GafPointCode;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.ICulvertBuilding;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Holger Albert
 */
public class ProfileObjectHelper
{
  private ProfileObjectHelper( )
  {
  }

  /**
   * Clones a profile object including all its metadata and all its records.
   * 
   * @param source
   *          The object that is going to be cloned.
   * @param targetProfile
   *          {@link IProfile} used to create target object.
   */
  public static IProfileObject cloneProfileObject( final IProfileObject source, final IProfile targetProfile )
  {
    return cloneProfileObject( source, targetProfile, source.getRecords().getAll() );
  }

  /**
   * Same as {@link #cloneProfileObject(IProfileObject, IProfile)}, but only clones the given source records.
   * 
   * @param sourceRecords
   *          The records that will be cloned into the new object. Must all be elements of the source.
   */
  public static IProfileObject cloneProfileObject( final IProfileObject source, final IProfile targetProfile, final IProfileObjectRecord[] sourceRecords )
  {
    final IProfileObject target = ProfileObjectFactory.createProfileObject( targetProfile, source.getType() );

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
    final IProfileObjectRecords targetRecords = target.getRecords();
    for( final IProfileObjectRecord sourceRecord : sourceRecords )
    {
      // TODO: assert that sourceRecord is a member of source

      final IProfileObjectRecord targetRecord = targetRecords.addNewRecord();
      targetRecord.setId( sourceRecord.getId() );
      targetRecord.setComment( sourceRecord.getComment() );
      targetRecord.setBreite( sourceRecord.getBreite() );
      targetRecord.setHoehe( sourceRecord.getHoehe() );
      targetRecord.setRechtswert( sourceRecord.getRechtswert() );
      targetRecord.setHochwert( sourceRecord.getHochwert() );
      targetRecord.setCode( sourceRecord.getCode() );
    }

    return target;
  }

  public static void updateObjectFromComponents( final IProfile source, final IProfileObject target, final String component )
  {
    final TupleResult sourceResult = source.getResult();
    final IComponent heightComponent = ProfileUtil.getFeatureComponent( component );
    final int heightIndex = sourceResult.indexOfComponent( heightComponent );

    // REMARK: happens e.g. for OK-GenericObjects if not connected to a bridge
    if( heightIndex == -1 )
      return;

    final IProfileObjectRecords targetRecords = target.getRecords();
    targetRecords.clearRecords();

    int recordCount = 0;
    for( final IRecord sourceRecord : sourceResult )
    {
      /* Only records with a height in the special component may be copied. */
      final Double height = (Double)sourceRecord.getValue( heightIndex );
      if( height == null )
        continue;

      /* Add a new record to the profile object. */
      final IProfileObjectRecord targetRecord = targetRecords.addNewRecord();

      /* Copy all standard values from the profile record to the profile object record. */
      org.kalypso.model.wspm.core.profil.ProfileObjectHelper.updateStandardProperties( sourceRecord, targetRecord );

      /* we create a new id */
      targetRecord.setId( Integer.toString( recordCount++ ) );

      /* Replace the height with the height from the special component. */
      targetRecord.setHoehe( height );
    }

    /* Guess the point codes. */
    guessCodes( targetRecords, component );
  }

  private static void guessCodes( final IProfileObjectRecords targetRecords, final String component )
  {
    for( int i = 0; i < targetRecords.size(); i++ )
    {
      final IProfileObjectRecord targetRecord = targetRecords.getRecord( i );

      if( component.equals( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE ) )
        targetRecord.setCode( getUkCode( i, targetRecords ) );

      if( component.equals( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE ) || component.equals( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ) )
        targetRecord.setCode( getOkCode( i, targetRecords ) );
    }
  }

  private static String getUkCode( final int i, final IProfileObjectRecords targetRecords )
  {
    if( i == 0 )
      return GafPointCode.UKAN.getKey();

    if( i == targetRecords.size() - 1 )
      return GafPointCode.UKEN.getKey();

    return GafPointCode.UKPP.getKey();
  }

  private static String getOkCode( final int i, final IProfileObjectRecords targetRecords )
  {
    if( i == 0 )
      return GafPointCode.OKAN.getKey();

    if( i == targetRecords.size() - 1 )
      return GafPointCode.OKEN.getKey();

    return GafPointCode.OKPP.getKey();
  }

  public static void updateObjectFromMetadata( final IProfile source, final ICulvertBuilding target, final Double height, final String fsCode, final String ukCode )
  {
    final Double bezugspunktX = target.getBezugspunktX();
    final Double bezugspunktY = target.getBezugspunktY();
    final Double breite = target.getBreite();
    if( Objects.isNull( bezugspunktX, bezugspunktY, breite ) )
      return;

    final Double hoehe = height != null ? height : breite;

    final GM_Point geoPosition = WspmProfileHelper.getGeoPosition( bezugspunktX.doubleValue(), source );

    final IProfileObjectRecords targetRecords = target.getRecords();
    targetRecords.clearRecords();

    final IProfileObjectRecord fsRecord = targetRecords.addNewRecord();
    fsRecord.setId( "1" ); //$NON-NLS-1$
    // fsRecord.setComment( null );
    fsRecord.setBreite( bezugspunktX );
    fsRecord.setHoehe( bezugspunktY );
    fsRecord.setRechtswert( geoPosition.getX() );
    fsRecord.setHochwert( geoPosition.getY() );
    fsRecord.setCode( fsCode );

    final IProfileObjectRecord ukRecord = targetRecords.addNewRecord();
    ukRecord.setId( "2" ); //$NON-NLS-1$
    // ukRecord.setComment( null );
    ukRecord.setBreite( bezugspunktX );
    ukRecord.setHoehe( bezugspunktY + hoehe );
    ukRecord.setRechtswert( geoPosition.getX() );
    ukRecord.setHochwert( geoPosition.getY() );
    ukRecord.setCode( ukCode );
  }
}