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
import org.kalypso.model.wspm.core.profil.util.ProfileUtil;
import org.kalypso.model.wspm.core.util.WspmProfileHelper;
import org.kalypso.model.wspm.pdb.gaf.IGafConstants;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.energyloss.EnergylossProfileObject;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingEi;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingKreis;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingMaul;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingTrapez;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.ICulvertBuilding;
import org.kalypso.model.wspm.tuhh.core.profile.sinuositaet.SinuositaetProfileObject;
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

  public static void updateObjectFromComponents( final IProfile source, final IProfileObject target, final String component )
  {
    final TupleResult sourceResult = source.getResult();
    final IComponent heightComponent = ProfileUtil.getFeatureComponent( component );
    final int heightIndex = sourceResult.indexOfComponent( heightComponent );

    final IProfileObjectRecords targetRecords = target.getRecords();
    targetRecords.clearRecords();

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

      /* Replace the height with the height from the special component. */
      targetRecord.setHoehe( height );
    }

    /* Guess the point codes. */
    guessCodes( targetRecords, component );
  }

  private static void guessCodes( final IProfileObjectRecords targetRecords, final String component )
  {
    for( int i = 0; i < targetRecords.getSize(); i++ )
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
      return IGafConstants.CODE_UKAN;

    if( i == targetRecords.getSize() - 1 )
      return IGafConstants.CODE_UKEN;

    return IGafConstants.CODE_UKPP;
  }

  private static String getOkCode( final int i, final IProfileObjectRecords targetRecords )
  {
    if( i == 0 )
      return IGafConstants.CODE_OKAN;

    if( i == targetRecords.getSize() - 1 )
      return IGafConstants.CODE_OKEN;

    return IGafConstants.CODE_OKPP;
  }

  public static void updateObjectFromMetadata( final IProfile source, final ICulvertBuilding target, final Double height, final String fsCode, final String ukCode )
  {
    final Double bezugspunktX = target.getBezugspunktX();
    final Double bezugspunktY = target.getBezugspunktY();
    final Double breite = target.getBreite();
    if( Objects.isNull( bezugspunktX, bezugspunktY, breite ) )
      return;

    /* REMARK: The width is defined as the durchmesser (circle), if no height is given. */
    final Double hoehe = height != null ? height : breite * 2;

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