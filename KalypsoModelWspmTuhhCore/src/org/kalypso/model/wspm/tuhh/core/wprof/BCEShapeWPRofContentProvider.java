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
package org.kalypso.model.wspm.tuhh.core.wprof;

import java.math.BigDecimal;

import org.apache.commons.lang.ObjectUtils;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * This importer knows how to interpret the BCE-W80 Shape format.
 *
 * @author Gernot Belger
 */
public class BCEShapeWPRofContentProvider implements IWProfPoint, IWspmTuhhConstants
{
  private final Feature m_feature;

  public BCEShapeWPRofContentProvider( final Feature feature )
  {
    m_feature = feature;
  }

  @Override
  public String getRiverId( )
  {
    return ObjectUtils.toString( m_feature.getProperty( "GEW_ID" ), "Unknown" ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public BigDecimal getStation( )
  {
// FIXME:
    final String stationString = ObjectUtils.toString( m_feature.getProperty( "GEW_ID" ), "-999999.9999" ); //$NON-NLS-1$ //$NON-NLS-2$
//    final String stationString = ObjectUtils.toString( m_feature.getProperty( "STATION" ), "-999999.9999" ); //$NON-NLS-1$ //$NON-NLS-2$
    final BigDecimal bigDecimal = new BigDecimal( stationString );
    bigDecimal.setScale( 4, BigDecimal.ROUND_HALF_UP );
    return bigDecimal;
  }

  @Override
  public GM_Point getLocation( )
  {
    return (GM_Point) m_feature.getProperty( "GEOM" ); //$NON-NLS-1$
  }

  @Override
  public String getComment( )
  {
    return ObjectUtils.toString( m_feature.getProperty( "P_KOMMENTA" ), "" ); //$NON-NLS-1$ //$NON-NLS-2$
//    return ""; //$NON-NLS-1$
  }

  @Override
  public BigDecimal getDistance( )
  {
    final String distanceString = ObjectUtils.toString( m_feature.getProperty( "DIST" ), "NaN" ); //$NON-NLS-1$ //$NON-NLS-2$
    final double distance = Double.parseDouble( distanceString );
    final BigDecimal bigDecimal = new BigDecimal( distance );
    bigDecimal.setScale( 4, BigDecimal.ROUND_HALF_UP );
    return bigDecimal;
  }

  @Override
  public double getValue( )
  {
    final String distanceString = ObjectUtils.toString( m_feature.getProperty( "Z" ), "NaN" ); //$NON-NLS-1$ //$NON-NLS-2$
    return Double.parseDouble( distanceString );
  }

  @Override
  public String getObjectType( )
  {
    return ObjectUtils.toString( m_feature.getProperty( "OBJ_TYP" ), "Unknown" ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public int getPunktattribut( )
  {
    final String typeString = ObjectUtils.toString( m_feature.getProperty( "PUNKTATTRI" ), "-1" ); //$NON-NLS-1$ //$NON-NLS-2$
    return Integer.parseInt( typeString );
  }

  @Override
  public int getProfileType( )
  {
    final String typeString = ObjectUtils.toString( m_feature.getProperty( "PROFILTYP" ), "-1" ); //$NON-NLS-1$ //$NON-NLS-2$
    return Integer.parseInt( typeString );
  }

  @Override
  public String[] getPhotoPathes( )
  {
    final String riverId = getRiverId();
    final String[] imageNames = getImageNames();
    if( imageNames == null || imageNames.length == 0 )
      return new String[] {};

    final String[] pathes = new String[imageNames.length];
    for( int i = 0; i < pathes.length; i++ )
      pathes[i] = String.format( "%5s\\Bilder\\%s", riverId, imageNames[i] ).toString(); //$NON-NLS-1$

    return pathes;
  }

  private String[] getImageNames( )
  {
    final String imagesString = ObjectUtils.toString( m_feature.getProperty( "P_FOTO" ), "" ); //$NON-NLS-1$ //$NON-NLS-2$
    return imagesString.split( ";" ); //$NON-NLS-1$
  }

  @Override
  public String getProfileComment( )
  {
    final String profileName = getProfileName();
    final String date = (String) m_feature.getProperty( "P_AUFNDATU" );
    return String.format( "Gew-ID: %s%nProfil-Name: %s%nErster Punkt: %s%nErster Obj_Typ: %s%nAufgenommen am: %s", getRiverId(), profileName, getComment(), getObjectType(), date ); //$NON-NLS-1$
  }

  @Override
  public String getProfileName( )
  {
    final String pNam = ObjectUtils.toString( m_feature.getProperty( "P_NAM" ), "-999999.9999" ); //$NON-NLS-1$ //$NON-NLS-2$
    final String pNamString = pNam.substring( pNam.indexOf( '-' ) + 1 );
    final String profileName = pNamString.replaceAll( "\"", "" ); //$NON-NLS-1$ //$NON-NLS-2$
    return profileName;
  }

  @Override
  public int getNumber( )
  {
    final String string = ObjectUtils.toString( m_feature.getProperty( "NUMMER" ), "-1" ); //$NON-NLS-1$ //$NON-NLS-2$
    return Integer.parseInt( string );
  }

  @Override
  public int getPartNumber( )
  {
    final String string = ObjectUtils.toString( m_feature.getProperty( "TEILPROFIL" ), "-1" ); //$NON-NLS-1$ //$NON-NLS-2$
    return Integer.parseInt( string );
  }
}
