package org.kalypso.util.runtime.args;

import java.text.DateFormat;
import java.util.Calendar;
import java.util.Date;

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
  public DateRangeArgument()
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
   * [now - pastDays, now]
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
    if( !(other instanceof DateRangeArgument) )
      throw new IllegalArgumentException( "Not comparing with a DateRangeArgument" );
    
    final DateRangeArgument dra = (DateRangeArgument) other;
    
    int cmp = this.m_from.compareTo( dra.m_from );
    if( cmp != 0 )
      return cmp;
    
    cmp = this.m_to.compareTo( dra.m_to );
    return cmp;
  }
}