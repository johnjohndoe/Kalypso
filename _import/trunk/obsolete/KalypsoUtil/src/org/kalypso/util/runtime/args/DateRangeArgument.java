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
package org.kalypso.util.runtime.args;

import java.text.DateFormat;
import java.util.Calendar;
import java.util.Date;

import org.apache.commons.lang.builder.HashCodeBuilder;
import org.kalypso.java.util.DateUtilities;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * Simple Date Range. Can be used as argument where IVariableArguments is
 * required.
 * 
 * @author schlienger
 */
public class DateRangeArgument implements IVariableArguments, Comparable
{
  private final Date m_from;

  private final Date m_to;

  /**
   * Simple constructor. Uses current date as from and to.
   */
  public DateRangeArgument( )
  {
    this( new Date(), new Date() );
  }

  /**
   * Consructor with longs
   * 
   * @param from
   * @param to
   */
  public DateRangeArgument( final long from, final long to )
  {
    this( new Date( from ), new Date( to ) );
  }

  /**
   * Constructor with Dates
   * 
   * @param from
   *          range from. If null minimum date is used (@see
   *          DateUtilities#getMinimum())
   * @param to
   *          range to. If null, current date is used.
   */
  public DateRangeArgument( Date from, Date to )
  {
    if( from == null )
      from = DateUtilities.getMinimum();

    if( to == null )
      to = new Date();

    m_from = from;
    m_to = to;
  }

  public Date getFrom( )
  {
    return m_from;
  }

  public Date getTo( )
  {
    return m_to;
  }

  /**
   * Returns true when this range contains the given date.
   * <p>
   * 
   * @param date
   * @return true if date is in [from, to]
   */
  public boolean contains( final Date date )
  {
    if( date == null )
      return false;

    return m_from.compareTo( date ) <= 0 && m_to.compareTo( date ) >= 0;
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString( )
  {
    DateFormat df = DateFormat.getDateTimeInstance();
    return df.format( m_from ) + " - " + df.format( m_to );
  }

  /**
   * Creates a <code>DateRangeArgument</code> containing the range:
   * 
   * <pre>
   * 
   *  [now - pastDays, now]
   *  
   * </pre>.
   * 
   * If pastDays == 0, then the range is null.
   * 
   * @param pastDays
   * @return new argument or null if pastDays is 0
   */
  public static DateRangeArgument createFromPastDays( final int pastDays )
  {
    if( pastDays == 0 )
      return null;

    final Calendar cal = Calendar.getInstance();

    final Date d2 = cal.getTime();

    cal.add( Calendar.DAY_OF_YEAR, -pastDays );

    final Date d1 = cal.getTime();

    return new DateRangeArgument( d1, d2 );
  }

  /**
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  public int compareTo( final Object other )
  {
    if( other == null )
      return 1;
    
    if( !(other instanceof DateRangeArgument) )
      throw new IllegalArgumentException(
          "Not comparing with a DateRangeArgument" );

    final DateRangeArgument dra = (DateRangeArgument) other;

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
  public int hashCode( )
  {
    final HashCodeBuilder hcb = new HashCodeBuilder();
    return hcb.append( m_from ).append( m_to ).hashCode();
  }
}