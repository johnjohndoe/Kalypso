package org.kalypso.convert.namodel.manager;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;

import org.kalypso.contribs.java.util.CalendarIterator;
import org.kalypso.contribs.java.util.CalendarUtilities;
import org.kalypso.contribs.java.util.ValueIterator;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.timeseries.NATimeSettings;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree_impl.gml.schema.GMLSchema;

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

  private final FeatureType m_IdleLanduseFT;

  final Hashtable m_idleLanduseTable = new Hashtable();

  private int m_idCounter = 0;

  public IdleLanduseManager( GMLSchema parameterSchema, NAConfiguration conf ) throws IOException
  {
    super( conf.getParameterFormatURL() );
    m_IdleLanduseFT = parameterSchema.getFeatureType( "IdealLandUse" );
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#mapID(int, org.kalypsodeegree.model.feature.FeatureType)
   */
  public String mapID( int id, FeatureType ft )
  {
    return null;
  }

  /**
   * 
   * @see org.kalypso.convert.namodel.manager.AbstractManager#parseFile(java.net.URL)
   */
  public Feature[] parseFile( URL url ) throws Exception
  {
    List result = new ArrayList();
    LineNumberReader reader = new LineNumberReader( new InputStreamReader( url.openConnection().getInputStream() ) );// new
    Feature fe = null;

    while( ( fe = readNextFeature( reader ) ) != null )
      result.add( fe );
    return (Feature[])result.toArray( new Feature[result.size()] );
  }

  private Feature readNextFeature( LineNumberReader reader ) throws Exception
  {
    HashMap propCollector = new HashMap();
    String line;
    // 9 (name is filedescription)
    line = reader.readLine();

    if( line == null )
      return null;
    System.out.println( reader.getLineNumber() + ": " + line );
    createProperties( propCollector, line, 9 );
    //Kommentarzeilen
    line = reader.readLine();
    line = reader.readLine();

    FeatureProperty prop = (FeatureProperty)propCollector.get( "name" );
    String fileDescription = (String)prop.getValue();
    final Feature feature;
    if( !m_idleLanduseTable.containsKey( fileDescription ) )
    {
      m_idCounter = m_idCounter + 1;
      Integer id = new Integer( m_idCounter );
      String asciiStringid = id.toString();
      m_idleLanduseTable.put( fileDescription, id );
      feature = getFeature( asciiStringid, m_IdleLanduseFT );
    }
    //IdleLanduse exists in another Landuse
    else
      return null;
    final String[] wtKcLaiAxis = new String[]
    {
        TimeserieConstants.TYPE_DATE,
        TimeserieConstants.TYPE_KC, // xkc
        TimeserieConstants.TYPE_WT, // xwt
        TimeserieConstants.TYPE_LAI }; // xlai

    final Object[][] values = new Object[12][4];
    final IAxis[] axis = TimeserieUtils.createDefaultAxes( wtKcLaiAxis, true );
    for( int i = 0; i < 12; i++ )
    {
      line = reader.readLine();
      System.out.println( "NutzParameter(" + i + "): " + line );
      createProperties( propCollector, line, 10 );

      // day.month.[-1|00]
      final String dateAsString = (String)( (FeatureProperty)propCollector.get( "dat" ) ).getValue();
      final String[] dateComponents = dateAsString.split( "\\." );
      int day = Integer.parseInt( dateComponents[0] );
      int month = Integer.parseInt( dateComponents[1] );
      int year = Integer.parseInt( dateComponents[2] );
      final Calendar calendar = NATimeSettings.getInstance().getCalendar();
      calendar.clear();
      calendar.set( Calendar.SECOND, 0 );
      calendar.set( Calendar.MINUTE, 0 );
      calendar.set( Calendar.HOUR_OF_DAY, 12 );
      calendar.set( Calendar.DATE, day );
      calendar.set( Calendar.MONTH, month - 1 );
      calendar.set( Calendar.YEAR, year + 2001 );

      FeatureProperty kcProp = (FeatureProperty)propCollector.get( "xkc" );
      FeatureProperty wtProp = (FeatureProperty)propCollector.get( "xwt" );
      FeatureProperty laiProp = (FeatureProperty)propCollector.get( "xlai" );
      Object xkc = kcProp.getValue();
      Object xwt = wtProp.getValue();
      Object xlai = laiProp.getValue();
      values[i][0] = calendar.getTime();
      values[i][1] = xkc;
      values[i][2] = xwt;
      values[i][3] = xlai;
    }

    //    final Calendar startDate = Calendar.getInstance();
    //    startDate.set( 2000, 11, 15 );
    //    final Calendar idealMonth = Calendar.getInstance();
    //    idealMonth.setTimeInMillis( 30 * 24 * 60 * 60 * 1000 );
    //    final Object intervall = new Date( idealMonth.getTimeInMillis() );
    //    final Object min = new Date( startDate.getTimeInMillis() );
    //    final int months = 12;
    //
    //    final Iterator iterator = new ValueIterator( min, intervall, months );
    //    for( int row = 0; row < months; row++ )
    //    {
    //      values[row][0] = iterator.next();
    //    }
    final ITuppleModel model = new SimpleTuppleModel( axis, values );
    final SimpleObservation observation = new SimpleObservation( null, null, fileDescription, true, null,
        new MetadataList(), axis, model );
    feature.setProperty( "idealLandUseZML", observation );

    line = reader.readLine();
    // continue reading
    Collection collection = propCollector.values();
    setParsedProperties( feature, collection );
    return feature;
  }

}
