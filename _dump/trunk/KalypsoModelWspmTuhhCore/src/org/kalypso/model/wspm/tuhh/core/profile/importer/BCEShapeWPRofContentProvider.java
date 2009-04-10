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
package org.kalypso.model.wspm.tuhh.core.profile.importer;

import java.math.BigDecimal;
import java.util.Formatter;

import org.apache.commons.lang.ObjectUtils;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * This importer knows how to interpret the BCE-W80 Shape format.
 *
 * @author Gernot Belger
 */
public class BCEShapeWPRofContentProvider implements IWProfContentProvider, IWspmTuhhConstants
{
  private final StringBuilder m_stringBuilder = new StringBuilder();

  private final Formatter m_formatter = new Formatter( m_stringBuilder );

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.importer.IWProfContentProvider#getRiverId(org.kalypsodeegree.model.feature.Feature)
   */
  @Override
  public String getRiverId( final Feature feature )
  {
    return ObjectUtils.toString( feature.getProperty( "GEW_ID" ), "Unknown" );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.importer.IWProfContentProvider#getStation(org.kalypsodeegree.model.feature.Feature)
   */
  @Override
  public BigDecimal getStation( final Feature feature )
  {
    final String stationString = ObjectUtils.toString( feature.getProperty( "MEASURE" ), "-999999.9999" );
    final BigDecimal bigDecimal = new BigDecimal( stationString );
    bigDecimal.setScale( 4, BigDecimal.ROUND_HALF_UP );
    return bigDecimal;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.importer.IWProfContentProvider#getLocation(org.kalypsodeegree.model.feature.Feature)
   */
  @Override
  public GM_Point getLocation( final Feature feature )
  {
    return (GM_Point) feature.getProperty( "GEOM" );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.importer.IWProfContentProvider#getComment(org.kalypsodeegree.model.feature.Feature)
   */
  @Override
  public String getComment( final Feature feature )
  {
    return ObjectUtils.toString( feature.getProperty( "P_KOMMENTA" ), "" );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.importer.IWProfContentProvider#getDistance(org.kalypsodeegree.model.feature.Feature)
   */
  @Override
  public BigDecimal getDistance( final Feature feature )
  {
    final String distanceString = ObjectUtils.toString( feature.getProperty( "DIST" ), "NaN" );
    final double distance = Double.parseDouble( distanceString );
    final BigDecimal bigDecimal = new BigDecimal( distance );
    bigDecimal.setScale( 4, BigDecimal.ROUND_HALF_UP );
    return bigDecimal;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.importer.IWProfContentProvider#getValue(org.kalypsodeegree.model.feature.Feature)
   */
  @Override
  public double getValue( final Feature feature )
  {
    final String distanceString = ObjectUtils.toString( feature.getProperty( "Z" ), "NaN" );
    return Double.parseDouble( distanceString );
  }

   /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.importer.IWProfContentProvider#getObjectType(org.kalypsodeegree.model.feature.Feature)
   */
  @Override
  public String getObjectType( final Feature feature )
  {
    return ObjectUtils.toString( feature.getProperty( "OBJ_TYP" ), "Unbekannt" );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.importer.IWProfContentProvider#getType(org.kalypsodeegree.model.feature.Feature)
   */
  @Override
  public int getType( final Feature feature )
  {
    final String typeString = ObjectUtils.toString( feature.getProperty( "PUNKTATTRI" ), "-1" );
    return Integer.parseInt( typeString );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.importer.IWProfContentProvider#getProfileType(org.kalypsodeegree.model.feature.Feature)
   */
  @Override
  public int getProfileType( final Feature feature )
  {
    final String typeString = ObjectUtils.toString( feature.getProperty( "PROFILTYP" ), "-1" );
    return Integer.parseInt( typeString );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.importer.IWProfContentProvider#getPhotoPath()
   */
  @Override
  public String getPhotoPath( final Feature feature )
  {
    final String riverId = getRiverId( feature );
    final String[] imageNames = getImageNames( feature );
    if( imageNames == null || imageNames.length == 0 )
      return null;

    final String format = String.format( "%5s\\Bilder\\%s", riverId, imageNames[0] ).toString();
    return format.replaceAll( " ", "0" );
  }

  private String[] getImageNames( final Feature feature )
  {
    final String imagesString = ObjectUtils.toString( feature.getProperty( "P_FOTO" ), "" );
    return imagesString.split( ";" );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.importer.IWProfContentProvider#getProfileComment(org.kalypsodeegree.model.feature.Feature)
   */
  @Override
  public String getProfileComment( final Feature feature )
  {
    final String profileName = getProfileName( feature );
    return String.format( "Gew-ID: %s%nProfil-Name: %s%nErster Punkt: %s%nErster Obj_Typ: %s%n", getRiverId( feature ), profileName, getComment( feature ), getObjectType( feature ) );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.importer.IWProfContentProvider#getProfileName(org.kalypsodeegree.model.feature.Feature)
   */
  @Override
  public String getProfileName( final Feature feature )
  {
    final String pNam = ObjectUtils.toString( feature.getProperty( "P_NAM" ), "-999999.9999" );
    final String pNamString = pNam.substring( pNam.indexOf( '-' ) + 1 );
    final String profileName = pNamString.replaceAll( "\"", "" );
    return profileName;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.importer.IWProfContentProvider#getNumber(org.kalypsodeegree.model.feature.Feature)
   */
  @Override
  public int getNumber( final Feature feature )
  {
    final String string = ObjectUtils.toString( feature.getProperty( "NUMMER" ), "-1" );
    return Integer.parseInt( string );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.importer.IWProfContentProvider#getPartNumber(org.kalypsodeegree.model.feature.Feature)
   */
  @Override
  public int getPartNumber( final Feature feature )
  {
    final String string = ObjectUtils.toString( feature.getProperty( "TEILPROFIL" ), "-1" );
    return Integer.parseInt( string );
  }
}
