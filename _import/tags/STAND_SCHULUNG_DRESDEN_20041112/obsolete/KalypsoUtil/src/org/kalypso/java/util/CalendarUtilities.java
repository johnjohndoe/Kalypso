package org.kalypso.java.util;

import java.util.Calendar;

/**
 * CalendarUtilities
 * 
 * @author schlienger
 */
public class CalendarUtilities
{
  /** do not instanciate */
  private CalendarUtilities( )
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
