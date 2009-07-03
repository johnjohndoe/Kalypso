package org.kalypso.convert.namodel;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.SimpleTimeZone;
import java.util.TimeZone;

/**
 * @author ich
 */
public class DummyTimeSeriesWriter
{
//  private final static double m_verdMin = 0;

//  private final static double m_verdMax = 1.0;

//  private final static int m_verdMaxDayOfYear = 30 * 3;

  private final Date m_start;

  private final Date m_end;

  private final static TimeZone m_timeZone = new SimpleTimeZone( 0, "ausgedacht" );

  private static DateFormat m_dateFormat = new SimpleDateFormat( "dd MM yyyy hh" );

  public static void main( String[] args )
  {
    long jetzt = new Date().getTime();
    long dt = 1000 * 60 * 24 * 365 * 2;
    DummyTimeSeriesWriter writer = new DummyTimeSeriesWriter( new Date( jetzt ), new Date( jetzt
        + dt ) );
    try
    {
      writer.writeTmpFile( new File( "C:\\TMP\\test.tmp" ) );
      writer.writeVerdFile( new File( "C:\\TMP\\test.ver" ) );
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
  }

  public DummyTimeSeriesWriter( Date start, Date end )
  {
    m_dateFormat.setCalendar( Calendar.getInstance( m_timeZone ) );
    m_start = start;
    m_end = end;
  }

  /**
   * Temperatur
   * 
   * @throws IOException
   */
  public void writeTmpFile( File tmpFile ) throws IOException
  {
    Writer writer = new FileWriter( tmpFile );
    writer.write( "EX2\n" ); //header
    Calendar calendarStart = Calendar.getInstance( m_timeZone );
    calendarStart.setTime( m_start );
    calendarStart.set( Calendar.DAY_OF_YEAR, 1 );
    calendarStart.set( Calendar.HOUR, 12 );

    Calendar calendarEnd = Calendar.getInstance( m_timeZone );
    calendarEnd.setTime( m_end );
    int writeTillYear = calendarEnd.get( Calendar.YEAR ) + 1;
    boolean goOn = true;
    while( goOn )
    {
      Date date = calendarStart.getTime();
      String out = m_dateFormat.format( date ) + " 10.0\n";
      writer.write( out );
      calendarStart.add( Calendar.DATE, 1 );
      int year = calendarStart.get( Calendar.YEAR );
      if( year > writeTillYear )
        goOn = false;
    }
    writer.close();
  }

  /**
   * Verdunstung
   */
  public void writeVerdFile( File tmpFile ) throws IOException
  {
    Writer writer = new FileWriter( tmpFile );
    writer.write( "EX2\n" ); //header
    Calendar calendarStart = Calendar.getInstance( m_timeZone );
    calendarStart.setTime( m_start );
    calendarStart.set( Calendar.DAY_OF_YEAR, 1 );
    calendarStart.set( Calendar.HOUR, 12 );
//    double daysinYear = calendarStart.getActualMaximum( Calendar.DAY_OF_YEAR );
    Calendar calendarEnd = Calendar.getInstance( m_timeZone );
    calendarEnd.setTime( m_end );
    int writeTillYear = calendarEnd.get( Calendar.YEAR ) + 1;
    boolean goOn = true;
    while( goOn )
    {
      Date date = calendarStart.getTime();
//      double dayinYear = calendarStart.get( Calendar.DAY_OF_YEAR );
//      double verd = ( m_verdMax + m_verdMin ) / 2.0d + ( m_verdMax - m_verdMin ) / 2.0d
//          * Math.sin( 2d * Math.PI / daysinYear * dayinYear + m_verdMaxDayOfYear );
      // TODO add numberformatparsing
      // String out = m_dateFormat.format( date )+" " +
      // Double.toString(verd)+"\n";
      String out = m_dateFormat.format( date ) + " " + "0.5\n";
      writer.write( out );
      calendarStart.add( Calendar.DATE, 1 );
      int year = calendarStart.get( Calendar.YEAR );
      if( year > writeTillYear )
        goOn = false;
    }
    writer.close();
  }
}