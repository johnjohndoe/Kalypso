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
import java.util.Date;
import java.util.TimeZone;

/**
 * Date utilities.
 * 
 * @author schlienger
 */
public final class DateUtilities
{
  private static Calendar calSrc = Calendar.getInstance();
  private static Calendar calDest = Calendar.getInstance();

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

  /**
   * Convert a date from one timezone to another timezone
   */
  public final static Date convert( final Date d, final TimeZone source, final TimeZone dest )
  {
    calSrc.setTimeZone( source );
    calDest.setTimeZone( dest );

    calSrc.setTimeInMillis( d.getTime() );

    calDest.clear();
    calDest.set( calSrc.get( Calendar.YEAR ), calSrc.get( Calendar.MONTH ), calSrc.get( Calendar.DAY_OF_MONTH ), calSrc
        .get( Calendar.HOUR_OF_DAY ), calSrc.get( Calendar.MINUTE ), calSrc.get( Calendar.SECOND ) );
    
    return calDest.getTime();
  }
}
