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
package org.kalypso.lhwsachsenanhalt.saale;

import java.io.IOException;
import java.io.LineNumberReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.Writer;
import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.StringTokenizer;
import java.util.TimeZone;
import java.util.TreeMap;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.request.IRequest;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.timeseries.interpolation.InterpolationFilter;

/**
 * Converter to write observations into a hwvor file.
 * <p>
 * Each observation will be filtered with an interpolation filter to exactly write 264 values with a step of 1 hour
 * </p>
 */
public class HWVOR00Converter
{
  public static final SimpleDateFormat HWVOR00_DATE = new SimpleDateFormat( "dd.M.yyyy H:mm" );

  static
  {
    // REMARK: we are parsing/writing the dates in the UTC-Timezone
    // So what we see in Kalypso (Timezone set to UTC for SachsenAnhalt) is what 
    // we get in the HWVOR00 files.
    // This corresponds also to what we see in the HWVOR00 programm directly (TODO: verify)
    HWVOR00_DATE.setTimeZone( TimeZone.getTimeZone( "UTC" ) );
  }

  /** datum -> Map(obsName -> value) */
  private final TreeMap m_obsMap = new TreeMap();

  private final ArrayList m_obsNames = new ArrayList();

  
  private final IRequest m_request;

  /**
   * @param currentTime
   *          The timeseries are written such: currentTime - 120h to currentTime + 148h step 1h
   */
  public HWVOR00Converter( final Date currentTime )
  {
    final Calendar cal = Calendar.getInstance();
    cal.setTime( currentTime );
    cal.add( Calendar.HOUR_OF_DAY, -120 );
    final Date startTime = cal.getTime();
    cal.add( Calendar.HOUR_OF_DAY, 120 + 144 );
    final Date stopTime = cal.getTime();

    m_request = new ObservationRequest( startTime, stopTime );
  }

  public boolean isEmpty()
  {
    return m_obsNames.isEmpty();
  }

  public void addObservation( final IObservation inObs, final String obsName, final String timeAxis,
      final String dataAxis )
  {
    try
    {
      final InterpolationFilter filter = new InterpolationFilter( Calendar.HOUR_OF_DAY, 1, true, "0", 0 );
      filter.initFilter( null, inObs, null );

      final ITuppleModel tplValues = filter.getValues( m_request );
      final IAxis axTime = ObservationUtilities.findAxisByType( tplValues.getAxisList(), timeAxis );
      final IAxis axData = ObservationUtilities.findAxisByType( tplValues.getAxisList(), dataAxis );

      for( int i = 0; i < tplValues.getCount(); i++ )
      {
        final Number value = (Number)tplValues.getElement( i, axData );

        /* Here rounding occurs */
        final BigDecimal adjustedValue = adjustValue( value, dataAxis );

        final Date date = (Date)tplValues.getElement( i, axTime );

        if( !m_obsMap.containsKey( date ) )
          m_obsMap.put( date, new HashMap() );

        ( (Map)m_obsMap.get( date ) ).put( obsName, adjustedValue );
      }
    }
    catch( final NoSuchElementException nse )
    {
      System.out.println( "Keine W/Q Beziehung für: " + obsName );
      //      nse.printStackTrace();
    }
    catch( final SensorException exp )
    {
      // TODO Exception handling verbessern
      //exp;
    }

    m_obsNames.add( obsName );
  }

  /**
   * Adjusts the value that will later be written out.
   * <p>
   * Mainly, rounding occurs here, depending on the unit of the value.
   * 
   */
  private static BigDecimal adjustValue( final Number number, final String type )
  {
    if( number == null )
      return null;

    final BigDecimal value = new BigDecimal( number.doubleValue() );

    if( type == TimeserieConstants.TYPE_RUNOFF )
      return value.setScale( 3, BigDecimal.ROUND_HALF_UP );

    if( type == TimeserieConstants.TYPE_VOLUME )
      return value.setScale( 2, BigDecimal.ROUND_HALF_UP );

    if( type == TimeserieConstants.TYPE_WATERLEVEL )
      return value.setScale( 0, BigDecimal.ROUND_HALF_UP );

    if( type == TimeserieConstants.TYPE_RAINFALL )
      return value.setScale( 2, BigDecimal.ROUND_HALF_UP );

    return value.setScale( 1, BigDecimal.ROUND_HALF_UP );
  }

  public void toHWVOR00( final Writer file )
  {
    final PrintWriter pWriter = new PrintWriter( file );

    //Die erste Zeile erzeugen
    pWriter.print( "Datum     " );
    for( final Iterator itr = m_obsNames.iterator(); itr.hasNext(); )
    {
      pWriter.print( "\t" );
      pWriter.print( (String)itr.next() );
    }

    pWriter.println();

    //Jede Zeile erzeugen, in der genügend Werte vorhanden sind
    for( final Iterator itr = m_obsMap.keySet().iterator(); itr.hasNext(); )
    {
      final Date currDate = (Date)itr.next();
      final Map valueMap = (Map)m_obsMap.get( currDate );

      pWriter.print( HWVOR00_DATE.format( currDate ) );
      
      for( final Iterator i = m_obsNames.iterator(); i.hasNext(); )
      {

        final String obsName = (String)i.next();
        final Number value = (Number)valueMap.get( obsName );

        final String valueStr = value == null ? "0.0" : value.toString();
        
        /* Write commata instead of points. HWVOR0 is able to read both, but , is better read by excel. */
        final String outStr = valueStr.replace( '.', ',' );
        
        pWriter.print( "\t" );
        pWriter.print( outStr );
      }

      pWriter.println();
    }
  }

  public static IObservation[] toZML( String valueType, Reader file ) throws ParseException, IOException
  {
    return toZML( valueType, file, null );
  }

  /**
   * @param file
   *          Will be wrapped into a buffered reader, so no need to do so by the caller.
   * @throws IOException
   */
  public static IObservation[] toZML( final String valueType, final Reader file, final MetadataList metadata )
      throws ParseException, IOException
  {

    // Die Datei einlesen!
    final ArrayList lines = new ArrayList();
    final LineNumberReader reader = new LineNumberReader( file );
    while( reader.ready() )
    {
      final String inputline = reader.readLine();
      if( inputline == null )
        break;

      lines.add( new StringTokenizer( inputline, "\t" ) );
    }

    ArrayList dates, data;
    ArrayList outOb = new ArrayList();

    IAxis[] axis;
    ITuppleModel tplValues;
    Object[][] tuppleData;
    String obsName;
    ( (StringTokenizer)lines.get( 0 ) ).nextToken();

    //Daten einlesen
    dates = new ArrayList();
    for( int i = 1; i != lines.size() && ( (StringTokenizer)lines.get( i ) ).hasMoreTokens(); i++ )
      dates.add( HWVOR00_DATE.parse( ( (StringTokenizer)lines.get( i ) ).nextToken() ) );

    while( ( (StringTokenizer)lines.get( 0 ) ).hasMoreTokens() )
    {
      data = new ArrayList();

      obsName = ( (StringTokenizer)lines.get( 0 ) ).nextToken();

      //Eine Spalte einlesen
      for( int i = 1; i != lines.size() && ( (StringTokenizer)lines.get( i ) ).hasMoreTokens(); i++ )
        data.add( new Double( ( (StringTokenizer)lines.get( i ) ).nextToken().replace( ',', '.' ) ) );

      //Achsen erzeugen
      axis = createAxis( valueType );

      tuppleData = new Object[dates.size()][2];
      for( int i = 0; i < dates.size(); i++ )
      {
        tuppleData[i][0] = dates.get( i );
        tuppleData[i][1] = data.get( i );
      }

      tplValues = new SimpleTuppleModel( axis, tuppleData );

      outOb.add( new SimpleObservation( "href", "ID", obsName, false, null, new MetadataList( metadata ), axis,
          tplValues ) );
    }
    return (IObservation[])outOb.toArray( new IObservation[outOb.size()] );
  }

  private static IAxis[] createAxis( final String sValueType )
  {
    final IAxis dateAxis = new DefaultAxis( "Datum", TimeserieConstants.TYPE_DATE, "", Date.class, true );
    //TimeserieUtils.getUnit( sValueType );
    final IAxis valueAxis = new DefaultAxis( TimeserieUtils.getName( sValueType ), sValueType, TimeserieUtils
        .getUnit( sValueType ), Double.class, false );
    final IAxis[] axis = new IAxis[]
    {
        dateAxis,
        valueAxis };
    return axis;
  }
}
