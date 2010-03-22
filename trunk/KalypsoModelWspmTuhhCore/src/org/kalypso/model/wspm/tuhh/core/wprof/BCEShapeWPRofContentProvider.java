/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import java.io.File;
import java.io.FilenameFilter;
import java.math.BigDecimal;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.DateFormat;
import java.util.Date;
import java.util.Properties;

import org.apache.commons.lang.ObjectUtils;
import org.kalypso.contribs.java.io.filter.PrefixSuffixFilter;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
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
  private static final String SPEC_Z = "Z"; //$NON-NLS-1$

  private static final String SPEC_P_NAM = "P_NAM"; //$NON-NLS-1$

  private static final String SPEC_NUMMER = "NUMMER"; //$NON-NLS-1$

  private final Feature m_feature;

  private final String m_photoContext;

  private final Properties m_specifiction;

  private final String m_pdfContext;

  private final WProfContextTokenReplacer m_tokenReplace;

  public BCEShapeWPRofContentProvider( final Feature feature, final WProfContextTokenReplacer tokenReplace, final String photoContext, final String pdfContext, final Properties specification )
  {
    m_feature = feature;
    m_tokenReplace = tokenReplace;
    m_photoContext = photoContext;
    m_pdfContext = pdfContext;
    m_specifiction = specification;
  }

  public <T> T getProperty( final String name, final Class<T> type, final T defaultValue )
  {
    final T value = getProperty( name, type );
    if( value != null )
      return value;

    return defaultValue;
  }

  private void checkPropertyExists( final String propertySpecificationName )
  {
    final IPropertyType propertyType = getPropertyType( propertySpecificationName );
    if( propertyType == null )
    {
      final String property = m_specifiction.getProperty( propertySpecificationName );
      final String msg = String.format( "Missing property '%s' in input shapefile.", property );
      throw new IllegalArgumentException( msg );
    }
  }

  public <T> T getProperty( final String propertySpecificationName, final Class<T> type )
  {
    final IPropertyType pt = getPropertyType( propertySpecificationName );
    if( pt == null )
      return null;

    final Object value = m_feature.getProperty( pt );
    return type.cast( value );
  }

  @SuppressWarnings("deprecation")
  private IPropertyType getPropertyType( final String propertySpecificationName )
  {
    final String property = m_specifiction.getProperty( propertySpecificationName );
    final IFeatureType featureType = m_feature.getFeatureType();
    return featureType.getProperty( property );
  }

  @Override
  public String getRiverId( )
  {
    final Object riverId = getProperty( "GEWAESSER_ID", Object.class, "Unbekannt" ); //$NON-NLS-1$ //$NON-NLS-2$
    return ObjectUtils.toString( riverId );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint#getRiverName()
   */
  @Override
  public String getRiverName( )
  {
    return "" + getProperty( "GEWAESSER_NAME", Object.class, "Unbekannt" ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public BigDecimal getStation( )
  {
    final double station = getProperty( "STATION", Double.class, Double.NaN ); //$NON-NLS-1$
    if( Double.isNaN( station ) || station < -99990.00 )
      return null;

    try
    {
// double faktorStation = 1000.0;
      final double faktorStation = 1;
      return new BigDecimal( station / faktorStation ).setScale( 4, BigDecimal.ROUND_HALF_UP );
    }
    catch( final NumberFormatException e )
    {
      e.printStackTrace();
      return null;
    }
  }

  @Override
  public GM_Point getLocation( )
  {
    return getProperty( "GEOM", GM_Point.class ); //$NON-NLS-1$
  }

  @Override
  public String getComment( )
  {
    return getProperty( "P_KOMMENTAR", String.class, "" ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public BigDecimal getDistance( )
  {
    checkPropertyExists( "DISTANCE" );//$NON-NLS-1$

    final double distance = getProperty( "DISTANCE", Double.class, Double.NaN ); //$NON-NLS-1$
    if( Double.isNaN( distance ) )
      return null;

    final BigDecimal bigDecimal = new BigDecimal( distance );
    bigDecimal.setScale( 4, BigDecimal.ROUND_HALF_UP );
    return bigDecimal;
  }

  @Override
  public double getValue( )
  {
    checkPropertyExists( SPEC_Z );
    return getProperty( SPEC_Z, Double.class, Double.NaN );
  }

  @Override
  public String getObjectType( )
  {
    return getProperty( "OBJECT_TYPE", String.class, "Unknown" ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public int getPunktattribut( )
  {
    return getProperty( "PUNKTATTRIBUT", Integer.class, -1 ); //$NON-NLS-1$
  }

  @Override
  public int getProfileType( )
  {
    return getProperty( "PROFILTYP", Integer.class, -1 ); //$NON-NLS-1$
  }

  @Override
  public URL[] getPhotos( )
  {
    try
    {
      final File photoDir = getContextDir( m_photoContext );
      if( photoDir == null || !photoDir.exists() || !photoDir.isDirectory() )
        return new URL[] {};

      final String[] photoNames = getPhotoNames( photoDir );

      final URL[] pathes = new URL[photoNames.length];
      for( int i = 0; i < pathes.length; i++ )
        pathes[i] = new File( photoDir, photoNames[i] ).toURI().toURL();

      return pathes;
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

  private String[] getPhotoNames( final File photoDir )
  {
    final String[] imageNames = getImageNames();
    if( imageNames == null || imageNames.length == 0 )
      return searchImages( photoDir );

    return imageNames;
  }

  private String[] searchImages( final File photoDir )
  {
    final String pNam = getPNam();
    final FilenameFilter photoFilter = new PrefixSuffixFilter( pNam, "" );
    return photoDir.list( photoFilter );
  }

  private File getContextDir( final String context )
  {
    if( context == null )
      return null;

    String result = context;
    final WProfContextToken[] tokens = m_tokenReplace.getTokens();
    for( final WProfContextToken token : tokens )
      result = token.replace( context, this );

    return new File( result );
  }

  private String[] getImageNames( )
  {
    final String imagesString = getProperty( "P_FOTO", String.class, "" ); //$NON-NLS-1$ //$NON-NLS-2$
    if( imagesString.trim().isEmpty() )
      return new String[0];

    return imagesString.split( ";", -1 ); //$NON-NLS-1$
  }

  public Date getDate( )
  {
    return getProperty( "P_AUFNDATUM", Date.class, null );
  }

  @Override
  public String getProfileComment( )
  {
    // FIXME
    final Date date = getDate();
    final String dateText = date == null ? "-" : DateFormat.getDateInstance( DateFormat.MEDIUM ).format( date );
// final String dateText = (String) m_feature.getProperty( "DATUM" );

    final String pnam = getPNam();

    final String pdfUrl = getPdfUrl();

    return String.format( "Gew-ID: %s%nProfilname: %s%nErster Punkt: %s%nErster Obj_Typ: %s%nAufgenommen am: %s%nPDF: %s ", getRiverId(), pnam, getComment(), getObjectType(), dateText, pdfUrl ); //$NON-NLS-1$
  }

  private String getPdfUrl( )
  {
    final File contextDir = getContextDir( m_pdfContext );
    final String pNam = getPNam();
    final File pdfFile = new File( contextDir, pNam + ".pdf" );

    try
    {
      final URL pdfURL = pdfFile.toURI().toURL();
      return pdfURL.toExternalForm();
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
      return "";
    }
  }

  public String getPNam( )
  {
    final String pnam = getProperty( SPEC_P_NAM, String.class, "Profilename unbekannt" ); //$NON-NLS-1$ //$NON-NLS-2$
    return pnam.replaceAll( "\"", "" );
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
    checkPropertyExists( SPEC_NUMMER ); //$NON-NLS-1$

    return getProperty( SPEC_NUMMER, Integer.class, -1 ); //$NON-NLS-1$
  }

  @Override
  public int getPartNumber( )
  {
    return getProperty( "TEILPROFIL", Integer.class, -1 ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint#getFeature()
   */
  @Override
  public Feature getFeature( )
  {
    return m_feature;
  }
}
