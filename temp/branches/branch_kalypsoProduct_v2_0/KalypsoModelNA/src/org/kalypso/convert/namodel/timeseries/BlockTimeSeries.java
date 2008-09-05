/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.convert.namodel.timeseries;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.LineNumberReader;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.SortedMap;
import java.util.TimeZone;
import java.util.TreeMap;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.kalypso.ui.KalypsoGisPlugin;

public class BlockTimeSeries
{
  private final DateFormat m_dateFormat;

  // private final DateFormat m_outputDateFormat = new SimpleDateFormat( "dd.MM.yyyy HH:mm:ss" );

  private final static int SEARCH_TIMEOFFSET = 0;

  private final static int SEARCH_BLOCK_HEADER = 1;

  private final static int SEARCH_VALUES = 2;

  // simulationszeitraum: von 970101 24 uhr bis 980102 24 uhr 24.000
  // simulationszeitraum: von 970101 24 uhr bis 102 24 uhr 24.000
  private final static Pattern pTime = Pattern.compile( ".+simulationszeitraum.+([0-9]{6}).+?([0-9]{1,2}).+[0-9]{3,6}.+[0-9]{1,2}.+?(\\d+\\.\\d+)\\D*" );

  // synth. n.: haufigkeit: 0.100 jahre,dauer: 13.00 h , zeitschr.: 0.083 h , verteilung: 2
  private final static Pattern pSynthTime = Pattern.compile( ".+synth. n..+haufigkeit:.+(\\d+\\.\\d+).+jahre,dauer.+(\\d+\\.\\d+).+h , zeitschr.:.+(\\d+\\.\\d+).+" );

  private final Pattern pBlock = Pattern.compile( "\\D*(\\d+)\\D+(\\d+)\\D+(\\d+)\\D*" );

  private final Pattern pHeader = Pattern.compile( "\\s*(-?+\\d+\\.\\d+)\\s*" );

  // private final Pattern pHeader = Pattern.compile( "\\D*(\\d+\\.\\d+)\\D*" );

  private final Hashtable<String, SortedMap> m_blocks;

  public BlockTimeSeries( final TimeZone timeZone )
  {
    SimpleDateFormat format = new SimpleDateFormat( "yyMMdd" );
    format.setTimeZone( timeZone );
    m_dateFormat = format;
    m_blocks = new Hashtable<String, SortedMap>();
  }

  /**
   * use TimeZone: CET - Central European Time
   */
  public BlockTimeSeries( )
  {
    this( KalypsoGisPlugin.getDefault().getDisplayTimeZone() );
// this( TimeZone.getTimeZone( "GMT+1" ) );
  }

  /**
   * imports all ts from given blockfile
   */
  public void importBlockFile( File blockFile )
  {
    importBlockFile( blockFile, null );
  }

  /**
   * imports all ts with allowed keys from blockfile
   */
  public void importBlockFile( File blockFile, Vector allowedKeys )
  {
    long startDate = 0;
    long timeStep = 0;
    int valuesToGo = 0;
    int valueIndex = 0;
    int valueOffset = 0;

    SortedMap<Date, String> timeSeries = null;
    try
    {
      LineNumberReader reader = new LineNumberReader( new FileReader( blockFile ) );
      String line;
      Matcher m = null;
      Matcher synthM = null;
      int step = SEARCH_TIMEOFFSET;
      while( (line = reader.readLine()) != null )
      {
        // System.out.println("LINE: "+line);
        if( !line.startsWith( "#" ) )
          switch( step )
          {
            case SEARCH_TIMEOFFSET:
              m = pTime.matcher( line );
              synthM = pSynthTime.matcher( line );
              if( m.matches() )
              {
                String sDate = m.group( 1 );
                String sTime = m.group( 2 );
                String sStep = m.group( 3 );
                final Date parseDate = m_dateFormat.parse( sDate );
                startDate = (parseDate).getTime();
                int sTime_int = Integer.parseInt( sTime );
                // 24 means 0 same day ! (RRM/fortran-logic)
                if( sTime_int == 24 )
                {
                  sTime = "0";
                }
                startDate += Long.parseLong( sTime ) * 1000l * 3600l;
                if( sStep.equals( "0.083" ) )
                {
                  /*
                   * timeStep = ((long) (sTimeStep_float * 1000f)) * 3600l;
                   */
                  timeStep = 300000l;
// System.out.println( "TimeStep: " + timeStep );
                }
                else
                {
                  timeStep = ((long) (Float.parseFloat( sStep ) * 1000f)) * 3600l;
                }

// Date testDate = new Date( startDate );
// System.out.println( "startdate: " + testDate + " step:" + sStep );
                step++;
              }
              if( synthM.matches() )
              {
                // synthetisches Ereignis hat kein Anfangsdatum, daher wird 01.01.2000 angenommen!
                String sDate = "000101";
                String sTime = "0";
                String sStep = synthM.group( 3 );
                final Date parseDate = m_dateFormat.parse( sDate );
                startDate = (parseDate).getTime();
                int sTime_int = Integer.parseInt( sTime );
                // 24 means 0 same day ! (RRM/fortran-logic)
                if( sTime_int == 24 )
                {
                  sTime = "0";
                }
                startDate += Long.parseLong( sTime ) * 1000l * 3600l;
                if( sStep.equals( "0.083" ) )
                {
                  /*
                   * timeStep = ((long) (sTimeStep_float * 1000f)) * 3600l;
                   */
                  timeStep = 300000l;
// System.out.println( "TimeStep: " + timeStep );
                }
                else
                {
                  timeStep = ((long) (Float.parseFloat( sStep ) * 1000f)) * 3600l;
                }

// Date testDate = new Date( startDate );
// System.out.println( "startdate: " + testDate + " step:" + sStep );
                step++;
              }
              break;
            case SEARCH_BLOCK_HEADER:
              m = pBlock.matcher( line );
              if( m.matches() )
              {
                String key = m.group( 1 );
                valuesToGo = Integer.parseInt( m.group( 3 ) );

                if( allowedKeys == null || allowedKeys.contains( key ) )
                {
                  if( m_blocks.containsKey( key ) )
                    timeSeries = m_blocks.get( key );
                  else
                  {
                    timeSeries = new TreeMap<Date, String>();
                    m_blocks.put( key, timeSeries );
                  }
                  step++;
                  valueIndex = 0;
                  valueOffset = timeSeries.size();
                }
              }
              break;
            case SEARCH_VALUES:
              String values[] = line.split( "\\s+" );
              for( int i = 0; i < values.length; i++ )
              {
                m = pHeader.matcher( values[i] );
                if( m.matches() )
                {
                  String value = m.group( 1 );
                  Date valueDate = new Date( startDate + (1 + valueIndex + valueOffset) * timeStep );
                  timeSeries.put( valueDate, value );
                  valueIndex += 1;
                  if( valueIndex >= valuesToGo )
                    step = SEARCH_BLOCK_HEADER;
                }
              }
              break;
            default:
              break;
          }
      }
      reader.close();
    }
    catch( Exception e )
    {
      e.printStackTrace();
      System.out.println( "could not read blockfile " );
    }
  }

  public Enumeration<String> getKeys( )
  {
    return m_blocks.keys();
  }

  public void exportToFile( final String key, final File exportFile, DateFormat dateFormat ) throws IOException
  {
    if( m_blocks.containsKey( key ) )
    {
      SortedMap map = m_blocks.get( key );

      FileWriter writer = new FileWriter( exportFile );
      String line;

      Iterator it = map.keySet().iterator();
      while( it.hasNext() )
      {
        Object dateKey = it.next();
        Object value = map.get( dateKey );
        line = dateFormat.format( (Date) dateKey ) + " " + value;
        writeln( writer, line );
      }
      writer.close();
    }
  }

  public TreeMap getTimeSerie( String key )
  {
    TreeMap resultData = (TreeMap) m_blocks.get( key );
    return resultData;
  }

  public void writeln( FileWriter writer, String line ) throws IOException
  {
    line = line + System.getProperty( "line.separator" );
    writer.write( line, 0, line.length() );
  }

  public boolean dataExistsForKey( String key )
  {
    return m_blocks.containsKey( key );
  }
}