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
package org.kalypso.contribs.java.util;

import java.util.Calendar;

/**
 * CalendarUtilities
 * 
 * @author schlienger
 */
public class CalendarUtilities
{
  /** do not instanciate */
  private CalendarUtilities()
  {
  // no instanciation
  }

  /**
   * Helper method that returns the calendar field according to its name.
   * <p>
   * Example: the name "DAY_OF_MONTH" will return Calendar.DAY_OF_MONTH
   * <p>
   * The comparison is not case sensitive.
   * 
   * @param fieldName
   * @return Calendar.*
   * @see java.util.Calendar
   */
  public final static int getCalendarField( final String fieldName )
  {
    if( "DATE".equalsIgnoreCase( fieldName ) )
      return Calendar.DATE;
    else if( "DAY_OF_MONTH".equalsIgnoreCase( fieldName ) )
      return Calendar.DAY_OF_MONTH;
    else if( "DAY_OF_WEEK".equalsIgnoreCase( fieldName ) )
      return Calendar.DAY_OF_WEEK;
    else if( "DAY_OF_WEEK_IN_MONTH".equalsIgnoreCase( fieldName ) )
      return Calendar.DAY_OF_WEEK_IN_MONTH;
    else if( "DAY_OF_YEAR".equalsIgnoreCase( fieldName ) )
      return Calendar.DAY_OF_YEAR;
    else if( "ERA".equalsIgnoreCase( fieldName ) )
      return Calendar.ERA;
    else if( "HOUR".equalsIgnoreCase( fieldName ) )
      return Calendar.HOUR;
    else if( "HOUR_OF_DAY".equalsIgnoreCase( fieldName ) )
      return Calendar.HOUR_OF_DAY;
    else if( "MILLISECOND".equalsIgnoreCase( fieldName ) )
      return Calendar.MILLISECOND;
    else if( "MINUTE".equalsIgnoreCase( fieldName ) )
      return Calendar.MINUTE;
    else if( "MONTH".equalsIgnoreCase( fieldName ) )
      return Calendar.MONTH;
    else if( "SECOND".equalsIgnoreCase( fieldName ) )
      return Calendar.SECOND;
    else if( "WEEK_OF_MONTH".equalsIgnoreCase( fieldName ) )
      return Calendar.WEEK_OF_MONTH;
    else if( "WEEK_OF_YEAR".equalsIgnoreCase( fieldName ) )
      return Calendar.WEEK_OF_YEAR;
    else if( "YEAR".equalsIgnoreCase( fieldName ) )
      return Calendar.YEAR;
    else if( "ZONE_OFFSET".equalsIgnoreCase( fieldName ) )
      return Calendar.ZONE_OFFSET;

    throw new IllegalArgumentException( "Calendar does not have a constant for: " + fieldName );
  }
}
