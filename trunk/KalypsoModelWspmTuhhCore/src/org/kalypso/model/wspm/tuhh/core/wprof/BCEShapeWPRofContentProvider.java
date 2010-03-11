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

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Properties;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
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
  private final Feature m_feature;

  private final URL m_photoContext;

  final Properties m_specifiction = new Properties();

  public BCEShapeWPRofContentProvider( final Feature feature, final URL photoContext )
  {
    m_feature = feature;
    m_photoContext = photoContext;

    readSpecification();
  }

  private void readSpecification( )
  {
    final URL resource = getClass().getResource( "BCEShapeWProfSpecification.ini" );
    InputStream is = null;
    try
    {
      is = resource.openStream();
      m_specifiction.load( is );
      is.close();
    }
    catch( final IOException e )
    {
      IOUtils.closeQuietly( is );
      e.printStackTrace();
    }
  }

  public <T> T getProperty( final String name, final Class<T> type, final T defaultValue )
  {
    final T value = getProperty( name, type );
    if( value != null )
      return value;

    return defaultValue;
  }

  @SuppressWarnings("deprecation")
  public <T> T getProperty( final String name, final Class<T> type )
  {
    final String property = m_specifiction.getProperty( name );

    final IFeatureType featureType = m_feature.getFeatureType();
    final IPropertyType pt = featureType.getProperty( property );
    if( pt == null )
    {
      // FIXME: zwischen optionalen und nicht unterscheiden
      return null;
    }

    final Object value = m_feature.getProperty( pt );
    return type.cast( value );
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
    final double distance = getProperty( "DISTANCE", Double.class, Double.NaN ); //$NON-NLS-1$
    final BigDecimal bigDecimal = new BigDecimal( distance );
    bigDecimal.setScale( 4, BigDecimal.ROUND_HALF_UP );
    return bigDecimal;
  }

  @Override
  public double getValue( )
  {
    return getProperty( "Z", Double.class, Double.NaN ); //$NON-NLS-1$
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
      final String[] photoPathes = getPhotoPathes();
      final String riverId = getRiverId();
      final String river5Id = String.format( "%5s", riverId ).replace( ' ', '0' );
      final String[] pathes = new String[photoPathes.length];
      for( int i = 0; i < pathes.length; i++ )
        pathes[i] = String.format( "%s\\Bilder\\%s", river5Id, photoPathes[i] ); //$NON-NLS-1$

      return createPhotoUrls( pathes );
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
      return searchImages();

    return imageNames;
  }

  private String[] searchImages( )
  {
    if( m_photoContext == null )
      return new String[] {};

    final File photoBaseDir = FileUtils.toFile( m_photoContext );
    if( photoBaseDir == null || !photoBaseDir.exists() || !photoBaseDir.isDirectory() )
      return new String[] {};

    final String riverId = getRiverId();
    final File photoRiverDir = new File( photoBaseDir, riverId );
    if( !photoRiverDir.exists() || !photoRiverDir.isDirectory() )
      return new String[] {};

    final File photoDir = new File( photoRiverDir, "Bilder" );
    if( !photoDir.exists() || !photoDir.isDirectory() )
      return new String[] {};

    final String pNam = getPNam();
    final FilenameFilter photoFilter = new PrefixSuffixFilter( pNam, "" );
    return photoDir.list( photoFilter );
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

    final String profileName = getProfileName();

    final String pdfUrl = getPdfUrl();

    return String.format( "Gew-ID: %s%nProfil-Name: %s%nErster Punkt: %s%nErster Obj_Typ: %s%nAufgenommen am: %s%nPDF: %s ", getRiverId(), profileName, getComment(), getObjectType(), dateText, pdfUrl ); //$NON-NLS-1$
  }

  private String getPdfUrl( )
  {
    final String riverId = getRiverId();
    final String river5Id = String.format( "%5s", riverId ).replace( ' ', '0' );
    final String pNam = getPNam();
    final String pdfPath = String.format( "%s\\Querprofile\\%s.pdf", river5Id, pNam ); //$NON-NLS-1$s

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

  public String getPNam( )
  {
    final String pnam = getProperty( "P_NAM", String.class, "Profilename unbekannt" ); //$NON-NLS-1$ //$NON-NLS-2$
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
    return getProperty( "NUMMER", Integer.class, -1 ); //$NON-NLS-1$
  }

  @Override
  public int getPartNumber( )
  {
    return getProperty( "TEILPROFIL", Integer.class, -1 ); //$NON-NLS-1$
  }
}
