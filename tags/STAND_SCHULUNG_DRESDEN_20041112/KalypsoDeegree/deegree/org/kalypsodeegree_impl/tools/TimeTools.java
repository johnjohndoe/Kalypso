/*----------------    FILE HEADER  ------------------------------------------
 
 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de
 
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
 
 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de
 
 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de
 
 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.tools;

import java.util.Calendar;
import java.util.GregorianCalendar;

public class TimeTools
{

  public static int YEAR = 0;

  public static int MONTH = 1;

  public static int DAY = 2;

  public static int HOUR = 3;

  public static int MINUTE = 4;

  public static int SECOND = 5;

  /**
   * get the current timestamp in ISO format
   */
  public static String getISOFormattedTime()
  {
    GregorianCalendar cal = new GregorianCalendar();
    StringBuffer sb = new StringBuffer( 200 );
    sb.append( cal.get( Calendar.YEAR ) + "-" );
    sb.append( ( cal.get( Calendar.MONTH ) + 1 ) + "-" );
    sb.append( cal.get( Calendar.DAY_OF_MONTH ) + " " );
    if( cal.get( Calendar.HOUR_OF_DAY ) < 10 )
    {
      sb.append( "0" + cal.get( Calendar.HOUR_OF_DAY ) + ":" );
    }
    else
    {
      sb.append( cal.get( Calendar.HOUR_OF_DAY ) + ":" );
    }
    if( cal.get( Calendar.MINUTE ) < 10 )
    {
      sb.append( "0" + cal.get( Calendar.MINUTE ) + ":" );
    }
    else
    {
      sb.append( cal.get( Calendar.MINUTE ) + ":" );
    }
    if( cal.get( Calendar.SECOND ) < 10 )
    {
      sb.append( "0" + cal.get( Calendar.SECOND ) + ":" );
    }
    else
    {
      sb.append( cal.get( Calendar.SECOND ) );
    }

    return sb.toString();
  }

  /**
   * get the current timestamp in ISO format
   */
  public static String getISOFormattedTime( Calendar cal )
  {
    StringBuffer sb = new StringBuffer( 200 );
    sb.append( cal.get( Calendar.YEAR ) + "-" );
    sb.append( ( cal.get( Calendar.MONTH ) + 1 ) + "-" );
    sb.append( cal.get( Calendar.DAY_OF_MONTH ) + " " );
    if( cal.get( Calendar.HOUR_OF_DAY ) < 10 )
    {
      sb.append( "0" + cal.get( Calendar.HOUR_OF_DAY ) + ":" );
    }
    else
    {
      sb.append( cal.get( Calendar.HOUR_OF_DAY ) + ":" );
    }
    if( cal.get( Calendar.MINUTE ) < 10 )
    {
      sb.append( "0" + cal.get( Calendar.MINUTE ) + ":" );
    }
    else
    {
      sb.append( cal.get( Calendar.MINUTE ) + ":" );
    }
    if( cal.get( Calendar.SECOND ) < 10 )
    {
      sb.append( "0" + cal.get( Calendar.SECOND ) + ":" );
    }
    else
    {
      sb.append( cal.get( Calendar.SECOND ) );
    }

    return sb.toString();
  }

  /**
   * returns a part of the submitted iso-formatted timestamp. possible values
   * for value are:
   * <ul>
   * <li>YEAR
   * <li>MONTH
   * <li>DAY
   * <li>HOUR
   * <li>MINUTE
   * <li>SECOND
   * </ul>
   */
  public static int get( int value, String isoTimestamp )
  {

    String s = null;
    isoTimestamp = isoTimestamp.trim();
    switch( value )
    {
    case 0:
      s = isoTimestamp.substring( 0, 4 );
      break;
    case 1:
      s = isoTimestamp.substring( 5, 7 );
      break;
    case 2:
      s = isoTimestamp.substring( 8, 10 );
      break;
    case 3:
    {
      if( isoTimestamp.length() >= 12 )
      {
        s = isoTimestamp.substring( 11, 13 );
      }
      else
        s = "12";
      break;
    }
    case 4:
    {
      if( isoTimestamp.length() >= 15 )
      {
        s = isoTimestamp.substring( 14, 16 );
      }
      else
        s = "0";
      break;
    }
    case 5:
    {
      if( isoTimestamp.length() >= 18 )
      {
        s = isoTimestamp.substring( 17, 19 );
      }
      else
        s = "0";
      break;
    }
    }
    return Integer.parseInt( s );
  }

  /**
   * returns a part of the submitted iso-formatted timestamp. possible values
   * for value are:
   * <ul>
   * <li>YEAR
   * <li>MONTH
   * <li>DAY
   * <li>HOUR
   * <li>MINUTE
   * <li>SECOND
   * </ul>
   */
  public static int getISO8601( int value, String isoTimestamp )
  {
    String s = null;
    isoTimestamp = isoTimestamp.trim();
    switch( value )
    {
    case 0:
      s = isoTimestamp.substring( 0, 4 );
      break;
    case 1:
      s = isoTimestamp.substring( 4, 6 );
      break;
    case 2:
      s = isoTimestamp.substring( 6, 8 );
      break;
    case 3:
    {
      if( isoTimestamp.length() >= 10 )
      {
        s = isoTimestamp.substring( 9, 11 );
      }
      else
        s = "12";
      break;
    }
    case 4:
    {
      if( isoTimestamp.length() >= 13 )
      {
        s = isoTimestamp.substring( 12, 14 );
      }
      else
        s = "0";
      break;
    }
    case 5:
    {
      if( isoTimestamp.length() >= 16 )
      {
        s = isoTimestamp.substring( 15, 17 );
      }
      else
        s = "0";
      break;
    }
    }
    return Integer.parseInt( s );
  }

  /**
   * creates an instance of a <tt>GregorianCalendar</tt> from an ISO timestamp
   */
  public static GregorianCalendar createCalendar( String isoDate )
  {
    int y = TimeTools.get( TimeTools.YEAR, isoDate );
    int m = TimeTools.get( TimeTools.MONTH, isoDate );
    int d = TimeTools.get( TimeTools.DAY, isoDate );
    int h = TimeTools.get( TimeTools.HOUR, isoDate );
    int min = TimeTools.get( TimeTools.MINUTE, isoDate );
    int sec = TimeTools.get( TimeTools.SECOND, isoDate );
    return new GregorianCalendar( y, m - 1, d, h, min, sec );
  }

  /**
   * creates an instance of a <tt>GregorianCalendar</tt> from an ISO 8601
   * timestamp
   */
  public static GregorianCalendar createCalendarISO8601( String isoDate )
  {
    int y = TimeTools.getISO8601( TimeTools.YEAR, isoDate );
    int m = TimeTools.getISO8601( TimeTools.MONTH, isoDate );
    int d = TimeTools.getISO8601( TimeTools.DAY, isoDate );
    int h = TimeTools.getISO8601( TimeTools.HOUR, isoDate );
    int min = TimeTools.getISO8601( TimeTools.MINUTE, isoDate );
    int sec = TimeTools.getISO8601( TimeTools.SECOND, isoDate );
    return new GregorianCalendar( y, m, d, h, min, sec );
  }

}