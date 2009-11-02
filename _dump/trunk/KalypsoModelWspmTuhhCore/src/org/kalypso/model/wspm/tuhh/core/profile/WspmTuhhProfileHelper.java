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
package org.kalypso.model.wspm.tuhh.core.profile;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InvalidObjectException;
import java.math.BigDecimal;
import java.math.RoundingMode;

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.serializer.ProfilSerializerUtilitites;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSink;
import org.kalypso.model.wspm.tuhh.core.wspwin.prf.PrfSource;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.Observation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * @author kimwerner
 */
public class WspmTuhhProfileHelper
{
  private WspmTuhhProfileHelper( )
  {
    // Helperclass, do not instantiate
  }

  public static final IProfil copyProfile( final IProfil profile ) throws InvalidObjectException
  {
    final ByteArrayOutputStream out = new ByteArrayOutputStream();
    try
    {
      ProfilSerializerUtilitites.writeProfile( new PrfSink(), profile, new BufferedOutputStream( out ) );
      final ByteArrayInputStream in = new ByteArrayInputStream( out.toByteArray() );
      final IProfil profil = ProfilSerializerUtilitites.readProfile( new PrfSource(), new BufferedInputStream( in ), profile.getType() );

      return profil;
    }
    catch( final IOException e )
    {
      throw new InvalidObjectException( e.getLocalizedMessage() );
    }
  }

  private final static BigDecimal valueToBigDecimal( final Object value )
  {
    return value instanceof Double ? new BigDecimal( (Double) value ).setScale( IProfileFeature.STATION_SCALE, RoundingMode.HALF_UP ) : new BigDecimal( Double.NaN );
  }

  public static final IObservation<TupleResult> profilesToLengthSection( final IProfileFeature[] profilFeatures )
  {
    TupleResult lsResult = new TupleResult();
    lsResult.addComponent( ProfilUtil.getFeatureComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_STATION ) );
    lsResult.addComponent( ProfilUtil.getFeatureComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_TYPE ) );
    lsResult.addComponent( ProfilUtil.getFeatureComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_GROUND ) );
    lsResult.addComponent( ProfilUtil.getFeatureComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_BOE_LI ) );
    lsResult.addComponent( ProfilUtil.getFeatureComponent( IWspmConstants.LENGTH_SECTION_PROPERT_BOE_RE ) );
    lsResult.addComponent( ProfilUtil.getFeatureComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_WEIR_OK ) );
    lsResult.addComponent( ProfilUtil.getFeatureComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_OK ) );
    lsResult.addComponent( ProfilUtil.getFeatureComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_UK ) );
    lsResult.addComponent( ProfilUtil.getFeatureComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_BRIDGE_WIDTH ) );
    lsResult.addComponent( ProfilUtil.getFeatureComponent( IWspmConstants.LENGTH_SECTION_PROPERTY_ROHR_DN ) );
    lsResult.addComponent( ProfilUtil.getFeatureComponent( IWspmConstants.POINT_PROPERTY_COMMENT ) );

    for( IProfileFeature profileFeature : profilFeatures )
    {
      final IProfil profil = profileFeature.getProfil();
      final IComponent profHei = profil.getPointPropertyFor( IWspmConstants.POINT_PROPERTY_HOEHE );
      final int indHei = profil.indexOfProperty( profHei );

      IRecord station = lsResult.createRecord();
      station.setValue( 10, profileFeature.getDescription() );
      station.setValue( 0, profileFeature.getBigStation(), true );// Station
      // Kennung
      // TODO: IWspmConstants.LENGTH_SECTION_PROPERTY_TYPE
      station.setValue( 2, ProfilUtil.stationToBigDecimal( ProfilUtil.getMinValueFor( profil, profHei ) ), true ); // Ground
      final IProfilPointMarker[] mbv = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_BORDVOLL );
      final IProfilPointMarker[] mtf = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );

      // Devider
      if( mbv.length == 2 )
      {
        station.setValue( 3, valueToBigDecimal( mbv[0].getPoint().getValue( indHei ) ), true ); // BOE_LI
        station.setValue( 4, valueToBigDecimal( mbv[1].getPoint().getValue( indHei ) ), true ); // BOE_RE
      }
      else if( mtf.length == 2 )
      {
        station.setValue( 3, valueToBigDecimal( mtf[0].getPoint().getValue( indHei ) ), true ); // BOE_LI
        station.setValue( 4, valueToBigDecimal( mtf[1].getPoint().getValue( indHei ) ), true ); // BOE_RE
      }

      // Profile Objects
      final IProfileObject[] profilObject = profil.getProfileObjects();
      if( profilObject != null && profilObject.length > 0 )
      {
        if( mtf.length < 2 )
        {
          station.setValue( 9, valueToBigDecimal( profilObject[0].getObjectProperty( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ) ), true );// ROHR_DN
        }
        else
        {
          final IRecord[] section = ProfilUtil.getInnerPoints( profil, mtf[0], mtf[1] ).toArray( new IRecord[] {} );
          if( profilObject[0] instanceof BuildingWehr )
          {
            station.setValue( 5, valueToBigDecimal( ProfilUtil.getSectionMaxValueFor( section, profil.hasPointProperty( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ) ) ), true ); // BridgeOK
          }
          if( profilObject[0] instanceof BuildingBruecke )
          {
            station.setValue( 7, valueToBigDecimal( ProfilUtil.getSectionMinValueFor( section, profil.hasPointProperty( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE ) ) ), true ); // BridgeUK
            station.setValue( 6, valueToBigDecimal( ProfilUtil.getSectionMaxValueFor( section, profil.hasPointProperty( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE ) ) ), true ); // BridgeOK
            station.setValue( 8, valueToBigDecimal( profilObject[0].getValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE ) ), true ); // BridgeWidth
          }
        }
      }

      lsResult.add( station );
    }
    return new Observation<TupleResult>( "root", "Profiles Length Section", lsResult );
  }
}
