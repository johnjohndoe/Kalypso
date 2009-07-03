package org.kalypso.convert.namodel.timeseries;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.LineNumberReader;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.SimpleTimeZone;
import java.util.SortedMap;
import java.util.TimeZone;
import java.util.TreeMap;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class BlockTimeSeries
{
  private final static DateFormat m_dateFormat = new SimpleDateFormat( "yyMMdd" );

  private final static DateFormat m_outputDateFormat = new SimpleDateFormat( "dd.MM.yyyy HH:mm:ss" );

  private final static int SEARCH_TIMEOFFSET = 0;

  private final static int SEARCH_BLOCK_HEADER = 1;

  private final static int SEARCH_VALUES = 2;

  private final static Pattern pTime = Pattern
      .compile( ".+simulationszeitraum.+([0-9]{6}).+?([0-9]{1,2}).+[0-9]{5,6}.+[0-9]{1,2}.+?(\\d+\\.\\d+)\\D*" );

  private final Pattern pBlock = Pattern.compile( "\\D*(\\d+)\\D+(\\d+)\\D+(\\d+)\\D*" );

  private final Pattern pHeader = Pattern.compile( "\\D*(\\d+\\.\\d+)\\D*" );

  private final Hashtable m_blocks;

  private static TimeZone m_timeZone = new SimpleTimeZone( 1000 * 60 * 60 * 2,
      "OmbrometerTimeZone" );

  public BlockTimeSeries()
  {
    Calendar calendar = Calendar.getInstance( m_timeZone );
    m_dateFormat.setTimeZone( m_timeZone );
    m_dateFormat.setCalendar( calendar );
    m_blocks = new Hashtable();
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

    SortedMap timeSeries = null;
    try
    {
      LineNumberReader reader = new LineNumberReader( new FileReader( blockFile ) );
      String line;
      Matcher m = null;
      int step = SEARCH_TIMEOFFSET;
      while( ( line = reader.readLine() ) != null )
      {
        //			System.out.println("LINE: "+line);
        if( !line.startsWith( "#" ) )
          switch( step )
          {
          case SEARCH_TIMEOFFSET:
            m = pTime.matcher( line );
            if( m.matches() )
            {
              String sDate = m.group( 1 );
              String sTime = m.group( 2 );
              String sStep = m.group( 3 );
              startDate = ( m_dateFormat.parse( sDate ) ).getTime();
              int sTime_int = Integer.parseInt( sTime );
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
                System.out.println( "TimeStep: " + timeStep );
              }
              else
              {
                timeStep = ( (long)( Float.parseFloat( sStep ) * 1000f ) ) * 3600l;
              }
              Date testDate = new Date( startDate );
              System.out.println( "startdate: " + testDate + "  step:" + sStep );
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
                  timeSeries = (TreeMap)m_blocks.get( key );
                else
                {
                  timeSeries = new TreeMap();
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
                Date valueDate = new Date( startDate + ( valueIndex + valueOffset ) * timeStep );
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

  public Enumeration getKeys()
  {
    return m_blocks.keys();
  }

  public void exportToFile( String key, File exportFile ) throws IOException
  {
    if( m_blocks.containsKey( key ) )
    {
      SortedMap map = (SortedMap)m_blocks.get( key );

      FileWriter writer = new FileWriter( exportFile );
      String line;

      Iterator it = map.keySet().iterator();
      while( it.hasNext() )
      {
        Object dateKey = it.next();
        Object value = (String)map.get( dateKey );
        line = m_outputDateFormat.format( (Date)dateKey ) + " " + value;
        writeln( writer, line );
      }
      writer.close();
    }
  }

  public TreeMap getTimeSerie( String key )
  {
    TreeMap resultData = (TreeMap)m_blocks.get( key );
    return resultData;
  }

  public void writeln( FileWriter writer, String line ) throws IOException
  {
    line = line + System.getProperty( "line.separator" );
    writer.write( line, 0, line.length() );
  }

  public boolean dataExistsForKey( String key )
  {
    return m_blocks.containsKey(key);
  }
}