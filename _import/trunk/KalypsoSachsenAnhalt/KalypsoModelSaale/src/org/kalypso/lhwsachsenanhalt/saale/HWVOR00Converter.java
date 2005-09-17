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
package org.kalypso.lhwsachsenanhalt.saale;

import java.io.IOException;
import java.io.LineNumberReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.Writer;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.StringTokenizer;
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
  private TreeMap m_obsMap;

  private ArrayList m_obsNames;

  private int m_obsNum = 0;

  public static final SimpleDateFormat HWVOR00_DATE = new SimpleDateFormat( "dd.M.yyyy H:mm" );

  private final IRequest m_request;

  /**
   * @param currentTime
   *          The timeseries are written such: currentTime - 120h to currentTime + 148h step 1h
   */
  public HWVOR00Converter( final Date currentTime )
  {
    final Calendar cal = Calendar.getInstance();
    cal.setTime( currentTime );
    cal.add( Calendar.HOUR_OF_DAY , -120 );
    final Date startTime = cal.getTime();
    cal.add( Calendar.HOUR_OF_DAY, 120 + 144 );
    final Date stopTime = cal.getTime();
    
    m_request = new ObservationRequest( startTime, stopTime );

    m_obsMap = new TreeMap();
    m_obsNames = new ArrayList();
    m_obsNum = 0;
  }

  public boolean isEmpty()
  {
    return m_obsNames.isEmpty();
  }

  public void addObservation( final IObservation inObs, final String obsName, final String timeAxis,
      final String dataAxis )
  {
    IAxis axTime;
    IAxis axData;
    ITuppleModel tplValues;
    Number value;
    Date date;

    m_obsNames.add( m_obsNum, obsName );

    try
    {
      final InterpolationFilter filter = new InterpolationFilter( Calendar.HOUR_OF_DAY, 1, true, 0, 0 );
      filter.initFilter( null, inObs, null );

      tplValues = filter.getValues( m_request );
      axTime = ObservationUtilities.findAxisByType( tplValues.getAxisList(), timeAxis );
      axData = ObservationUtilities.findAxisByType( tplValues.getAxisList(), dataAxis );

      for( int i = 0; i < tplValues.getCount(); i++ )
      {
        value = (Number)tplValues.getElement( i, axData );
        date = (Date)tplValues.getElement( i, axTime );

        if( m_obsMap.containsKey( date ) )
        {
          ( (ArrayList)m_obsMap.get( date ) ).add( m_obsNum, value );
        }
        else if( m_obsNum == 0 )
        {
          m_obsMap.put( date, new ArrayList() );
          ( (ArrayList)m_obsMap.get( date ) ).add( 0, value );
        }
      }
    }
    catch( final SensorException exp )
    {
      // TODO Exception handling verbessern
      //exp;
    }
    m_obsNum++;
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

    //Jede Zeile erzeugen, in der gen¸gend Werte vorhanden sind
    for( final Iterator itr = m_obsMap.keySet().iterator(); itr.hasNext(); )
    {
      final Date currDate = (Date)itr.next();
      if( ( (ArrayList)m_obsMap.get( currDate ) ).size() >= m_obsNum )
      {
        pWriter.print( HWVOR00_DATE.format( currDate ) );
        final ArrayList currList = (ArrayList)m_obsMap.get( currDate );
        for( Iterator i = currList.iterator(); i.hasNext(); )
        {
          pWriter.print( "\t" );
          pWriter.print( ( (Number)i.next() ).toString() );
        }

        pWriter.println();
      }
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
