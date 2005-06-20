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
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.Set;
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
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;

public class HWVOR00Converter
{
  private TreeMap m_obsMap;

  private ArrayList m_obsNames;

  private int m_obsNum = 0;

  public static final SimpleDateFormat HWVOR00_DATE = new SimpleDateFormat( "dd.M.yyyy H:mm" );

  public HWVOR00Converter()
  {
    m_obsMap = new TreeMap();
    m_obsNames = new ArrayList();
    m_obsNum = 0;
  }

  public void addObservation( final IObservation inObs, final String timeAxis, final String dataAxis )
  {
    IAxis axTime;
    IAxis axData;
    ITuppleModel tplValues;
    Number value;
    Date date;

    m_obsNames.add( m_obsNum, inObs.getName() );

    try
    {
      tplValues = inObs.getValues( null );
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
    catch( SensorException exp )
    {
      // TODO Exception handling verbessern
      //exp;
    }
    m_obsNum++;
  }

  public void toHWVOR00( Writer file )
  {
    PrintWriter pWriter = new PrintWriter( file );
    Set keys;
    ArrayList currList;
    String output;
    Date currDate;

    //Die erste Zeile erzeugen
    output = "Datum     ";
    for( Iterator itr = m_obsNames.iterator(); itr.hasNext(); )
      output += "\t" + (String)itr.next();

    pWriter.println( output );

    //Jede Zeile erzeugen, in der genügend Werte vorhanden sind
    keys = m_obsMap.keySet();
    for( Iterator itr = keys.iterator(); itr.hasNext(); )
    {
      currDate = (Date)itr.next();
      if( ( (ArrayList)m_obsMap.get( currDate ) ).size() >= m_obsNum )
      {
        output = HWVOR00_DATE.format( currDate );
        currList = (ArrayList)m_obsMap.get( currDate );
        for( Iterator i = currList.iterator(); i.hasNext(); )
          output += "\t" + ( (Number)i.next() ).toString();

        pWriter.println( output );
      }
    }
  }

  public static IObservation[] toZML( String valueType, Reader file ) throws ParseException
  {
    return toZML( valueType, file, null );
  }

  public static IObservation[] toZML( String valueType, Reader file, MetadataList metadata ) throws ParseException
  {
    ArrayList lines = new ArrayList();
    ArrayList dates, data;
    ArrayList outOb = new ArrayList();
    LineNumberReader reader = new LineNumberReader( file );

    IAxis[] axis;
    ITuppleModel tplValues;
    Object[][] tuppleData;
    String obsName;
    String inputline;

    // Die Datei einlesen!
    try
    {
      inputline = reader.readLine();
      while( inputline != null )
      {
        lines.add( new StringTokenizer( inputline, "\t" ) );
        inputline = reader.readLine();
      }
    }
    catch( IOException exp )
    {
      exp.printStackTrace();
    }
    finally
    {
      try
      {
        reader.close();
      }
      catch( IOException e )
      {
        e.printStackTrace();
      }
    }

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
      outOb.add( new SimpleObservation( "href", "ID", obsName, false, null, metadata, axis, tplValues ) );
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
