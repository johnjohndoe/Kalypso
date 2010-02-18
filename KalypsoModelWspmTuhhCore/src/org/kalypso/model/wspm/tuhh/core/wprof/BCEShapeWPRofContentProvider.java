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
import java.net.MalformedURLException;
import java.net.URL;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;

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

  private final URL m_photoContext;

  public BCEShapeWPRofContentProvider( final Feature feature, final URL photoContext )
  {
    m_feature = feature;
    m_photoContext = photoContext;
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
  public URL[] getPhotos( )
  {
    try
    {
      final String[] photoPathes = getPhotoPathes();
      return createPhotoUrls( photoPathes );
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
//      final String message = String.format( "Unable to create profile at %s", getStation() ); //$NON-NLS-1$
// final Status status = new Status( IStatus.ERROR, KalypsoModelWspmTuhhCorePlugin.getID(), message, e );
// throw new CoreException( status );
      return new URL[] {};
    }
  }

  private String[] getPhotoPathes( )
  {
    final String[] imageNames = getImageNames();
    if( imageNames == null || imageNames.length == 0 )
      return new String[] {};

    final String riverId = getRiverId();
    final String[] pathes = new String[imageNames.length];
    for( int i = 0; i < pathes.length; i++ )
      pathes[i] = String.format( "%5s\\Bilder\\%s", riverId, imageNames[i] ); //$NON-NLS-1$

    return pathes;
  }

  private URL[] createPhotoUrls( final String[] photoPathes ) throws MalformedURLException
  {
    final Collection<URL> urls = new ArrayList<URL>( photoPathes.length );

    for( final String photoPathe : photoPathes )
    {
      final URL photoURL = new URL( m_photoContext, photoPathe );
      urls.add( photoURL );
    }

    return urls.toArray( new URL[urls.size()] );
  }

  private String[] getImageNames( )
  {
    final String imagesString = ObjectUtils.toString( m_feature.getProperty( "P_FOTO" ), "" ); //$NON-NLS-1$ //$NON-NLS-2$
    return imagesString.split( ";" ); //$NON-NLS-1$
  }

  @Override
  public String getProfileComment( )
  {
    final Date date = (Date) m_feature.getProperty( "P_AUFNDATU" );

    final String profileName = getProfileName();
    final String dateText = DateFormat.getDateInstance( DateFormat.MEDIUM ).format( date );
    
    final String pdfUrl = getPdfUrl();

    return String.format( "Gew-ID: %s%nProfil-Name: %s%nErster Punkt: %s%nErster Obj_Typ: %s%nAufgenommen am: %s%nPDF: %s", getRiverId(), profileName, getComment(), getObjectType(), dateText, pdfUrl ); //$NON-NLS-1$
  }

  private String getPdfUrl( )
  {
    final String riverId = getRiverId();
    final String pNam = getPNam();
    final String pdfPath = String.format( "%5s\\Querprofile\\%s.pdf", riverId, pNam ); //$NON-NLS-1$
    
    try
    {
      final URL pdfURL = new URL( m_photoContext, pdfPath );
      return pdfURL.toExternalForm();
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
      return "";
    }
  }

  private String getPNam()
  {
    return ObjectUtils.toString( m_feature.getProperty( "P_NAM" ), "-999999.9999" ); //$NON-NLS-1$ //$NON-NLS-2$
  }
  
  @Override
  public String getProfileName( )
  {
    final String pNam = getPNam();
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
