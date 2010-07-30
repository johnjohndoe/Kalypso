package org.kalypso.convert.namodel.manager;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;

import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.convert.namodel.i18n.Messages;
import org.kalypso.convert.namodel.timeseries.NATimeSettings;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypsodeegree.model.feature.Feature;

/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
/*
 * 
 * @author huebsch
 */
public class IdleLanduseManager extends AbstractManager
{

  private final IFeatureType m_IdleLanduseFT;

  private final IFeatureType m_landuseFT;

  private final IFeatureType m_sealingFT;

  final Hashtable<String, Integer> m_idleLanduseTable = new Hashtable<String, Integer>();

  private int m_idCounter = 0;

  public IdleLanduseManager( final org.kalypso.gmlschema.GMLSchema parameterSchema, final NAConfiguration conf ) throws IOException
  {
    super( conf.getParameterFormatURL() );
    m_IdleLanduseFT = parameterSchema.getFeatureType( NaModelConstants.PARA_IDEAL_LANDUSE );
    m_landuseFT = parameterSchema.getFeatureType( NaModelConstants.PARA_LANDUSE );
    m_sealingFT = parameterSchema.getFeatureType( NaModelConstants.PARA_SEALING );
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#mapID(int, org.kalypsodeegree.model.feature.FeatureType)
   */
  @Override
  public String mapID( final int id, final IFeatureType ft )
  {
    return null;
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#parseFile(java.net.URL)
   */
  @Override
  public Feature[] parseFile( final URL url ) throws Exception
  {
    final List<Feature> result = new ArrayList<Feature>();
    final LineNumberReader reader = new LineNumberReader( new InputStreamReader( url.openConnection().getInputStream() ) );// new
    Feature fe = null;

    while( (fe = readNextFeature( reader )) != null )
      result.add( fe );
    return result.toArray( new Feature[result.size()] );
  }

  private Feature readNextFeature( final LineNumberReader reader ) throws Exception
  {
    final HashMap<String, String> propCollector = new HashMap<String, String>();
    String line;
    // 9 (name is filedescription)
    line = reader.readLine();

    if( line == null )
      return null;
    System.out.println( reader.getLineNumber() + ": " + line ); //$NON-NLS-1$
    createProperties( propCollector, line, 9 );
    // Kommentarzeilen
    line = reader.readLine();
    line = reader.readLine();

    final String fileDescription = propCollector.get( "name" ); //$NON-NLS-1$
    final Feature feature;
    if( !m_idleLanduseTable.containsKey( fileDescription ) )
    {
      m_idCounter = m_idCounter + 1;
      final Integer id = new Integer( m_idCounter );
      final String asciiStringid = id.toString();
      m_idleLanduseTable.put( fileDescription, id );
      feature = getFeature( asciiStringid, m_IdleLanduseFT );
    }
    // IdleLanduse exists in another Landuse
    else
      return null;
    final String[] wtKcLaiAxis = new String[] { TimeserieConstants.TYPE_DATE, TimeserieConstants.TYPE_KC, TimeserieConstants.TYPE_WT, TimeserieConstants.TYPE_LAI };

    final Object[][] values = new Object[12][4];
    final IAxis[] axis = TimeserieUtils.createDefaultAxes( wtKcLaiAxis, true );
    for( int i = 0; i < 12; i++ )
    {
      line = reader.readLine();
      System.out.println( Messages.getString( "org.kalypso.convert.namodel.manager.IdleLanduseManager.2", i, line ) ); //$NON-NLS-1$ //$NON-NLS-2$
      createProperties( propCollector, line, 10 );

      // day.month.[-1|00]
      final String dateAsString = propCollector.get( "dat" ); //$NON-NLS-1$
      final String[] dateComponents = dateAsString.split( "\\." ); //$NON-NLS-1$
      final int day = Integer.parseInt( dateComponents[0] );
      final int month = Integer.parseInt( dateComponents[1] );
      final int year = Integer.parseInt( dateComponents[2] );
      final Calendar calendar = NATimeSettings.getInstance().getCalendar();
      calendar.clear();
      calendar.set( Calendar.SECOND, 0 );
      calendar.set( Calendar.MINUTE, 0 );
      calendar.set( Calendar.HOUR_OF_DAY, 12 );
      calendar.set( Calendar.DATE, day );
      calendar.set( Calendar.MONTH, month - 1 );
      calendar.set( Calendar.YEAR, year + 2001 );
      final Object xkc = propCollector.get( "xkc" ); //$NON-NLS-1$
      final Object xwt = propCollector.get( "xwt" ); //$NON-NLS-1$
      final Object xlai = propCollector.get( "xlai" ); //$NON-NLS-1$
      values[i][0] = calendar.getTime();
      values[i][1] = xkc;
      values[i][2] = xwt;
      values[i][3] = xlai;
    }

    final ITuppleModel model = new SimpleTuppleModel( axis, values );
    final SimpleObservation observation = new SimpleObservation( null, fileDescription, new MetadataList(), model );
    feature.setProperty( NaModelConstants.PARA_IDEAL_LANDUSE_ZML, observation );

    line = reader.readLine();
    // continue reading
    // Collection collection = propCollector.values();
    setParsedProperties( feature, propCollector, null );
    return feature;
  }

  public Feature[] parseFilecsv( final URL url ) throws Exception
  {
    final List<Feature> result = new ArrayList<Feature>();
    final LineNumberReader reader = new LineNumberReader( new InputStreamReader( url.openConnection().getInputStream() ) );// new
    Feature fe = null;
    while( (fe = readNextcsvFeature( reader )) != null )
      result.add( fe );
    return result.toArray( new Feature[result.size()] );

  }

  public Feature readNextcsvFeature( final LineNumberReader reader2 ) throws IOException
  {

    final HashMap<String, String> landusePropCollector = new HashMap<String, String>();
    String line;
    line = reader2.readLine();
    if( line == null )
      return null;
    final String[] strings = line.split( ";" ); //$NON-NLS-1$
    System.out.println( line );
    final String landuse = strings[0];
    final Feature feature = getFeature( landuse, m_landuseFT );
    landusePropCollector.put( "name", landuse ); //$NON-NLS-1$
    final String description = strings[1];
    landusePropCollector.put( "description", description ); //$NON-NLS-1$
    final String sealinglink = strings[2];
    final Feature sealingFE = getFeature( sealinglink, m_sealingFT );
    landusePropCollector.put( "sealingLink", sealingFE.getId() ); //$NON-NLS-1$
    feature.setProperty( NaModelConstants.PARA_LANDUSE_PROP_SEALING_LINK, sealingFE.getId() );
    final String landuseperiodlink = strings[3];
    final Feature idleLanduseFE = getFeature( landuseperiodlink, m_IdleLanduseFT );
    landusePropCollector.put( "idealLandUsePeriodLink", idleLanduseFE.getId() ); //$NON-NLS-1$
    feature.setProperty( NaModelConstants.PARA_LANDUSE_PROP_LANDUSE_LINK, idleLanduseFE.getId() );

    setParsedProperties( feature, landusePropCollector, null );
    return feature;
  }

  public Feature[] parseSealingFilecsv( final URL url ) throws IOException
  {
    final List<Feature> result = new ArrayList<Feature>();
    final LineNumberReader reader = new LineNumberReader( new InputStreamReader( url.openConnection().getInputStream() ) );// new
    Feature fe = null;
    while( (fe = readNextsealingFeature( reader )) != null )
      result.add( fe );
    return result.toArray( new Feature[result.size()] );

  }

  public Feature readNextsealingFeature( final LineNumberReader reader3 ) throws IOException
  {

    final HashMap<String, String> sealingPropCollector = new HashMap<String, String>();
    String line;
    line = reader3.readLine();
    if( line == null )
      return null;
    final String[] strings = line.split( ";" ); //$NON-NLS-1$
    System.out.println( line );
    final String sealing = strings[0];
    final Feature feature = getFeature( sealing, m_sealingFT );
    sealingPropCollector.put( "name", "Klasse_" + sealing ); //$NON-NLS-1$ //$NON-NLS-2$
    final String description = strings[1];
    sealingPropCollector.put( "description", description ); //$NON-NLS-1$
    final String vers = strings[2];
    sealingPropCollector.put( "m_vers", vers ); //$NON-NLS-1$

    setParsedProperties( feature, sealingPropCollector, null );
    return feature;
  }

}
