package org.kalypso.java.util;

import java.util.Calendar;
import java.util.Date;

/**
 * Date utilities.
 * 
 * @author schlienger
 */
public final class DateUtilities
{
  private DateUtilities()
  {
    // not intended to be instanciated
  }
  
  /**
   * @return the minimum Date that the Calendar can deliver.
   */
  public final static Date getMinimum()
  {
    final Calendar cal = Calendar.getInstance();
    
    final int yearMin = cal.getMinimum( Calendar.YEAR );
    final int monthMin = cal.getMinimum( Calendar.MONTH );
    final int dayMin = cal.getMinimum( Calendar.DAY_OF_MONTH );
    final int hourMin = cal.getMinimum( Calendar.HOUR_OF_DAY );
    final int minMin = cal.getMinimum( Calendar.MINUTE );
    final int secMin = cal.getMinimum( Calendar.SECOND );

    cal.clear();
    cal.set( yearMin, monthMin, dayMin, hourMin, minMin, secMin );
    
    return cal.getTime();
  }
}
