/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.ogc.sensor.diagview.jfreechart;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.TimeZone;

import org.jfree.chart.axis.DateTickUnit;
import org.jfree.chart.axis.TickUnitSource;
import org.jfree.chart.axis.TickUnits;
import org.kalypso.core.KalypsoCorePlugin;

/**
 * Axis for the date data type. Also does the default configeration like setting the Kalypso-Timezone and so on.
 * 
 * @author Gernot Belger
 */
public class DateAxis extends org.jfree.chart.axis.DateAxis
{
  public DateAxis( final String label )
  {
    super( label, KalypsoCorePlugin.getDefault().getTimeZone() );

    // REMARK: the next line is necessary, as the constructor with timezone does
    // not initalize the timeline (freechart bug!?)
    setTimeline( new DefaultTimeline() );
    // Create standard source with correct timezone
    final TickUnitSource source = createStandardDateTickUnits( KalypsoCorePlugin.getDefault().getTimeZone() );
    setStandardTickUnits( source );
  }

  /**
   * Resets the timezone of this date axis. Considers tick-units as well as date-format overrides.
   */
  public void setTimezone( final TimeZone timezone )
  {
    final DateFormat df = getDateFormatOverride() == null ? null : getDateFormatOverride();
    if( df != null )
    {
      df.setTimeZone( timezone );
      setDateFormatOverride( df );
    }

    final TickUnitSource source = createStandardDateTickUnits( timezone );
    setStandardTickUnits( source );
  }
  

  /**
   * Special tick units for kalypso
   */
  public static TickUnitSource createStandardDateTickUnits( final TimeZone zone )
  {
    if( zone == null )
      throw new IllegalArgumentException( "Null 'zone' argument." );

    final TickUnits units = new TickUnits();

    // date formatters
    //      DateFormat f1 = new SimpleDateFormat("HH:mm:ss.SSS");
    //      DateFormat f2 = new SimpleDateFormat("HH:mm:ss");
    //      DateFormat f3 = new SimpleDateFormat("HH:mm");
    //      DateFormat f4 = new SimpleDateFormat("d-MMM, HH:mm");
    //      DateFormat f5 = new SimpleDateFormat("d-MMM");
    //      DateFormat f6 = new SimpleDateFormat("MMM-yyyy");
    //      DateFormat f7 = new SimpleDateFormat("yyyy");

    final DateFormat f1 = new SimpleDateFormat( "dd.MM HH:mm:ss.SSS" );
    final DateFormat f2 = new SimpleDateFormat( "dd.MM HH:mm:ss" );
    final DateFormat f3 = new SimpleDateFormat( "dd.MM HH:mm" );
    final DateFormat f4 = new SimpleDateFormat( "dd.MM HH:mm" );
    final DateFormat f5 = new SimpleDateFormat( "dd.MM" );
    final DateFormat f6 = new SimpleDateFormat( "dd.MM.yy" );
    final DateFormat f7 = new SimpleDateFormat( "yyyy" );

    f1.setTimeZone( zone );
    f2.setTimeZone( zone );
    f3.setTimeZone( zone );
    f4.setTimeZone( zone );
    f5.setTimeZone( zone );
    f6.setTimeZone( zone );
    f7.setTimeZone( zone );

    // milliseconds
    units.add( new DateTickUnit( DateTickUnit.MILLISECOND, 1, f1 ) );
    units.add( new DateTickUnit( DateTickUnit.MILLISECOND, 5, DateTickUnit.MILLISECOND, 1, f1 ) );
    units.add( new DateTickUnit( DateTickUnit.MILLISECOND, 10, DateTickUnit.MILLISECOND, 1, f1 ) );
    units.add( new DateTickUnit( DateTickUnit.MILLISECOND, 25, DateTickUnit.MILLISECOND, 5, f1 ) );
    units.add( new DateTickUnit( DateTickUnit.MILLISECOND, 50, DateTickUnit.MILLISECOND, 10, f1 ) );
    units.add( new DateTickUnit( DateTickUnit.MILLISECOND, 100, DateTickUnit.MILLISECOND, 10, f1 ) );
    units.add( new DateTickUnit( DateTickUnit.MILLISECOND, 250, DateTickUnit.MILLISECOND, 10, f1 ) );
    units.add( new DateTickUnit( DateTickUnit.MILLISECOND, 500, DateTickUnit.MILLISECOND, 50, f1 ) );

    // seconds
    units.add( new DateTickUnit( DateTickUnit.SECOND, 1, DateTickUnit.MILLISECOND, 50, f2 ) );
    units.add( new DateTickUnit( DateTickUnit.SECOND, 5, DateTickUnit.SECOND, 1, f2 ) );
    units.add( new DateTickUnit( DateTickUnit.SECOND, 10, DateTickUnit.SECOND, 1, f2 ) );
    units.add( new DateTickUnit( DateTickUnit.SECOND, 30, DateTickUnit.SECOND, 5, f2 ) );

    // minutes
    units.add( new DateTickUnit( DateTickUnit.MINUTE, 1, DateTickUnit.SECOND, 5, f3 ) );
    units.add( new DateTickUnit( DateTickUnit.MINUTE, 2, DateTickUnit.SECOND, 10, f3 ) );
    units.add( new DateTickUnit( DateTickUnit.MINUTE, 5, DateTickUnit.MINUTE, 1, f3 ) );
    units.add( new DateTickUnit( DateTickUnit.MINUTE, 10, DateTickUnit.MINUTE, 1, f3 ) );
    units.add( new DateTickUnit( DateTickUnit.MINUTE, 15, DateTickUnit.MINUTE, 5, f3 ) );
    units.add( new DateTickUnit( DateTickUnit.MINUTE, 20, DateTickUnit.MINUTE, 5, f3 ) );
    units.add( new DateTickUnit( DateTickUnit.MINUTE, 30, DateTickUnit.MINUTE, 5, f3 ) );

    // hours
    units.add( new DateTickUnit( DateTickUnit.HOUR, 1, DateTickUnit.MINUTE, 5, f3 ) );
    units.add( new DateTickUnit( DateTickUnit.HOUR, 2, DateTickUnit.MINUTE, 10, f3 ) );
    units.add( new DateTickUnit( DateTickUnit.HOUR, 4, DateTickUnit.MINUTE, 30, f3 ) );
    units.add( new DateTickUnit( DateTickUnit.HOUR, 6, DateTickUnit.HOUR, 1, f3 ) );
    units.add( new DateTickUnit( DateTickUnit.HOUR, 12, DateTickUnit.HOUR, 1, f4 ) );

    // days
    units.add( new DateTickUnit( DateTickUnit.DAY, 1, DateTickUnit.HOUR, 1, f5 ) );
    units.add( new DateTickUnit( DateTickUnit.DAY, 2, DateTickUnit.HOUR, 1, f5 ) );
    units.add( new DateTickUnit( DateTickUnit.DAY, 3, DateTickUnit.HOUR, 1, f5 ) );
    units.add( new DateTickUnit( DateTickUnit.DAY, 4, DateTickUnit.HOUR, 1, f5 ) );
    units.add( new DateTickUnit( DateTickUnit.DAY, 5, DateTickUnit.HOUR, 1, f5 ) );
    units.add( new DateTickUnit( DateTickUnit.DAY, 6, DateTickUnit.HOUR, 1, f5 ) );
    units.add( new DateTickUnit( DateTickUnit.DAY, 7, DateTickUnit.DAY, 1, f5 ) );
    units.add( new DateTickUnit( DateTickUnit.DAY, 10, DateTickUnit.DAY, 1, f5 ) );
    units.add( new DateTickUnit( DateTickUnit.DAY, 15, DateTickUnit.DAY, 1, f5 ) );

    // months
    units.add( new DateTickUnit( DateTickUnit.MONTH, 1, DateTickUnit.DAY, 1, f6 ) );
    units.add( new DateTickUnit( DateTickUnit.MONTH, 2, DateTickUnit.DAY, 1, f6 ) );
    units.add( new DateTickUnit( DateTickUnit.MONTH, 3, DateTickUnit.MONTH, 1, f6 ) );
    units.add( new DateTickUnit( DateTickUnit.MONTH, 4, DateTickUnit.MONTH, 1, f6 ) );
    units.add( new DateTickUnit( DateTickUnit.MONTH, 6, DateTickUnit.MONTH, 1, f6 ) );

    // years
    units.add( new DateTickUnit( DateTickUnit.YEAR, 1, DateTickUnit.MONTH, 1, f7 ) );
    units.add( new DateTickUnit( DateTickUnit.YEAR, 2, DateTickUnit.MONTH, 3, f7 ) );
    units.add( new DateTickUnit( DateTickUnit.YEAR, 5, DateTickUnit.YEAR, 1, f7 ) );
    units.add( new DateTickUnit( DateTickUnit.YEAR, 10, DateTickUnit.YEAR, 1, f7 ) );
    units.add( new DateTickUnit( DateTickUnit.YEAR, 25, DateTickUnit.YEAR, 5, f7 ) );
    units.add( new DateTickUnit( DateTickUnit.YEAR, 50, DateTickUnit.YEAR, 10, f7 ) );
    units.add( new DateTickUnit( DateTickUnit.YEAR, 100, DateTickUnit.YEAR, 20, f7 ) );

    return units;
  }

}
