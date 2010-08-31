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
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.SimpleTimeZone;
import java.util.TimeZone;

/**
 * @author doemming
 */
public class DummyTimeSeriesWriter
{
  private final static double VERD_MIN = 0.25;

  private final static double VERD_MAX = 3.0;

  private final Date m_start;

  private final Date m_end;

  private final static TimeZone m_timeZone = new SimpleTimeZone( 0, "ausgedacht" ); //$NON-NLS-1$

  private static DateFormat m_dateFormat = new SimpleDateFormat( "dd MM yyyy 12 " ); //$NON-NLS-1$

  private static DecimalFormat m_numberFoirmat = new DecimalFormat( "##.###" ); //$NON-NLS-1$

  public static void main( String[] args )
  {
    long jetzt = new Date().getTime();
    long dt = 1000 * 60 * 24 * 365 * 2;
    DummyTimeSeriesWriter writer = new DummyTimeSeriesWriter( new Date( jetzt ), new Date( jetzt + dt ) );
    try
    {
      writer.writeTmpFile( new File( "C:\\TMP\\test.tmp" ) ); //$NON-NLS-1$
      writer.writeVerdFile( new File( "C:\\TMP\\test.ver" ) ); //$NON-NLS-1$
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
    m_numberFoirmat.setGroupingUsed(false);
    DecimalFormatSymbols decimalFormatSymbols = m_numberFoirmat.getDecimalFormatSymbols();
    decimalFormatSymbols.setDecimalSeparator('.');
    m_numberFoirmat.setDecimalFormatSymbols(decimalFormatSymbols);
    //    decimalFormatSymbols.set 
  }

  /**
   * Temperatur
   * 
   * @throws IOException
   */
  public void writeTmpFile( File tmpFile ) throws IOException
  {
    Writer writer = new FileWriter( tmpFile );
    writer.write( "EX2\n" ); //header //$NON-NLS-1$
    Calendar calendarStart = Calendar.getInstance( m_timeZone );
    calendarStart.setTime( m_start );
    calendarStart.set( Calendar.DAY_OF_YEAR, 0 );
    calendarStart.set( Calendar.HOUR, 12 );

    Calendar calendarEnd = Calendar.getInstance( m_timeZone );
    calendarEnd.setTime( m_end );
    int writeTillYear = calendarEnd.get( Calendar.YEAR ) + 1;
    boolean goOn = true;
    while( goOn )
    {
      Date date = calendarStart.getTime();
      writer.write( m_dateFormat.format( date ) );
      writer.write( "1.0\n" ); //$NON-NLS-1$
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
    writer.write( "EX2\n" ); //header //$NON-NLS-1$
    Calendar calendarStart = Calendar.getInstance( m_timeZone );
    calendarStart.setTime( m_start );
    calendarStart.set( Calendar.DAY_OF_YEAR, 0 );
    calendarStart.set( Calendar.HOUR, 12 );
    Calendar calendarEnd = Calendar.getInstance( m_timeZone );
    calendarEnd.setTime( m_end );
    int writeTillYear = calendarEnd.get( Calendar.YEAR ) + 1;
    boolean goOn = true;
    while( goOn )
    {
      final Date date = calendarStart.getTime();
      writer.write( m_dateFormat.format( date ) );
      final Calendar cal = Calendar.getInstance();
      cal.setTime( date );
      final double dayOfYear = cal.get( Calendar.DAY_OF_YEAR );
      double value = ( VERD_MAX + VERD_MIN ) / 2d - Math.cos( 2d * Math.PI / 365d * dayOfYear )
          * ( VERD_MAX - VERD_MIN ) / 2d;
      writer.write( m_numberFoirmat.format( value ) );
      //      writer.write( Double.toString( value ) );
      //            writer.write( "0.5");
      writer.write( "\n" ); //$NON-NLS-1$
      calendarStart.add( Calendar.DATE, 1 );
      int year = calendarStart.get( Calendar.YEAR );
      if( year > writeTillYear )
        goOn = false;
    }
    writer.close();
  }
}