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
package org.kalypso.ogc.sensor;

import java.text.DateFormat;
import java.util.Calendar;
import java.util.Date;

import org.apache.commons.lang.builder.HashCodeBuilder;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;

/**
 * Simple Date Range
 * 
 * @author schlienger
 */
public class DateRange implements Comparable
{
  private final Date m_from;

  private final Date m_to;

  /**
   * Simple constructor. Uses current date as from and to.
   */
  public DateRange()
  {
    this( null, null );
  }

  /**
   * Consructor with longs
   */
  public DateRange( final long from, final long to )
  {
    this( new Date( from ), new Date( to ) );
  }

  /**
   * Constructor with Dates
   * 
   * @param from
   *          if null, current date is used
   * @param to
   *          if null, current date is used.
   */
  public DateRange( final Date from, final Date to )
  {
    if( from == null )
      m_from = new Date();
    else
      m_from = from;

    if( to == null )
      m_to = new Date();
    else
      m_to = to;
  }

  public Date getFrom()
  {
    return m_from;
  }

  public Date getTo()
  {
    return m_to;
  }

  /**
   * Returns true when this range contains the given date.
   * <p>
   * 
   * @param date
   * @return true if date is in ]from, to[
   */
  public boolean containsExclusive( final Date date )
  {
    if( date == null )
      return false;

    return m_from.compareTo( date ) < 0 && m_to.compareTo( date ) > 0;
  }

  /**
   * Returns true when this range contains the given date.
   * <p>
   * 
   * @param date
   * @return true if date is in [from, to]
   */
  public boolean containsInclusive( final Date date )
  {
    if( date == null )
      return false;

    return m_from.compareTo( date ) <= 0 && m_to.compareTo( date ) >= 0;
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    final DateFormat df = TimeserieUtils.getDateFormat();
    return df.format( m_from ) + " - " + df.format( m_to );
  }

  /**
   * Creates a <code>DateRangeArgument</code> containing the range:
   * 
   * <pre>
   *  [now - pastDays, now]
   * </pre>.
   * 
   * If pastDays == 0, then the range is null.
   * 
   * @return new argument or null if pastDays is 0
   */
  public static DateRange createFromPastDays( final int pastDays )
  {
    if( pastDays == 0 )
      return null;

    final Calendar cal = Calendar.getInstance();

    final Date d2 = cal.getTime();

    cal.add( Calendar.DAY_OF_YEAR, -pastDays );

    final Date d1 = cal.getTime();

    return new DateRange( d1, d2 );
  }

  /**
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  public int compareTo( final Object other )
  {
    if( other == null )
      return 1;

    if( !( other instanceof DateRange ) )
      throw new IllegalArgumentException( "Not comparing with a DateRangeArgument" );

    final DateRange dra = (DateRange)other;

    int cmp = this.m_from.compareTo( dra.m_from );
    if( cmp != 0 )
      return cmp;

    cmp = this.m_to.compareTo( dra.m_to );
    return cmp;
  }

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  public boolean equals( Object obj )
  {
    return compareTo( obj ) == 0;
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  public int hashCode()
  {
    return new HashCodeBuilder().append( m_from ).append( m_to ).toHashCode();
  }
}