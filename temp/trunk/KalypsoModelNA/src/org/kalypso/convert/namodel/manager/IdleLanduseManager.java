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

import org.kalypso.contribs.java.util.ValueIterator;
import org.kalypso.convert.namodel.NAConfiguration;
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
/*
 * 
 * @author huebsch
 */
public class IdleLanduseManager extends AbstractManager
{

  private final IFeatureType m_IdleLanduseFT;

  final Hashtable m_idleLanduseTable = new Hashtable();

  private int m_idCounter = 0;

  public IdleLanduseManager( org.kalypso.gmlschema.GMLSchema parameterSchema, NAConfiguration conf ) throws IOException
  {
    super( conf.getParameterFormatURL() );
    m_IdleLanduseFT = parameterSchema.getFeatureType( "IdealLandUse" );
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#mapID(int, org.kalypsodeegree.model.feature.FeatureType)
   */
  public String mapID( int id, IFeatureType ft )
  {
    return null;
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#parseFile(java.net.URL)
   */
  public Feature[] parseFile( URL url ) throws Exception
  {
    List result = new ArrayList();
    LineNumberReader reader = new LineNumberReader( new InputStreamReader( url.openConnection().getInputStream() ) );// new
    Feature fe = null;

    while( (fe = readNextFeature( reader )) != null )
      result.add( fe );
    return (Feature[]) result.toArray( new Feature[result.size()] );
  }

  private Feature readNextFeature( LineNumberReader reader ) throws Exception
  {
    final HashMap<String, String> propCollector = new HashMap<String, String>();
    String line;
    // 9 (name is filedescription)
    line = reader.readLine();

    if( line == null )
      return null;
    System.out.println( reader.getLineNumber() + ": " + line );
    createProperties( propCollector, line, 9 );
    // Kommentarzeilen
    line = reader.readLine();
    line = reader.readLine();

    // FeatureProperty prop = (FeatureProperty)propCollector.get( "name" );
    String fileDescription = propCollector.get( "name" );
    final Feature feature;
    if( !m_idleLanduseTable.containsKey( fileDescription ) )
    {
      m_idCounter = m_idCounter + 1;
      Integer id = new Integer( m_idCounter );
      String asciiStringid = id.toString();
      m_idleLanduseTable.put( fileDescription, id );
      feature = getFeature( asciiStringid, m_IdleLanduseFT );
    }
    // IdleLanduse exists in another Landuse
    else
      return null;
    final String[] wtKcLaiAxis = new String[] { TimeserieConstants.TYPE_DATE, TimeserieConstants.TYPE_LAI, TimeserieConstants.TYPE_WT, TimeserieConstants.TYPE_KC };

    final Object[][] values = new Object[12][4];
    final IAxis[] axis = TimeserieUtils.createDefaultAxes( wtKcLaiAxis, true );
    for( int i = 0; i < 12; i++ )
    {
      line = reader.readLine();
      System.out.println( "NutzParameter(" + i + "): " + line );
      createProperties( propCollector, line, 10 );

      // FeatureProperty kcProp = (FeatureProperty) propCollector.get( "xkc" );
      // FeatureProperty wtProp = (FeatureProperty) propCollector.get( "xwt" );
      // FeatureProperty laiProp = (FeatureProperty) propCollector.get( "xlai" );

      Object xkc = propCollector.get( "xkc" );
      Object xwt = propCollector.get( "xwt" );
      Object xlai = propCollector.get( "xlai" );

      values[i][1] = xkc;
      values[i][2] = xwt;
      values[i][3] = xlai;
    }

    final Calendar startDate = Calendar.getInstance();
    startDate.set( 2000, 11, 15 );
    final Calendar idealMonth = Calendar.getInstance();
    idealMonth.setTimeInMillis( 30 * 24 * 60 * 60 * 1000 );
    final Object intervall = new Date( idealMonth.getTimeInMillis() );
    final Object min = new Date( startDate.getTimeInMillis() );
    final int months = 12;

    final Iterator iterator = new ValueIterator( min, intervall, months );
    for( int row = 0; row < months; row++ )
    {
      values[row][0] = iterator.next();
    }
    final ITuppleModel model = new SimpleTuppleModel( axis, values );
    SimpleObservation observation = new SimpleObservation( null, null, fileDescription, true, null, new MetadataList(), axis, model );
    feature.setProperty( "idealLandUseZML", observation );

    line = reader.readLine();
    // continue reading
//    Collection collection = propCollector.values();
    setParsedProperties( feature, propCollector,null);
    return feature;
  }

}
