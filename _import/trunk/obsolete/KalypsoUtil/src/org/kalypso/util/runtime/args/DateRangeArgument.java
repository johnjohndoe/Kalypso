package org.kalypso.util.runtime.args;

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
public class DateRangeArgument implements IVariableArguments
{
  private final Date m_from;

  private final Date m_to;

  /**
   * 
   * @param from
   * @param to
   */
  public DateRangeArgument( final long from, final long to )
  {
    this( new Date(from), new Date(to) );
  }
  
  /**
   * @param from if null, minimum date is used
   * @param to if null, current date is used
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

  public Date getFrom()
  {
    return m_from;
  }

  public Date getTo()
  {
    return m_to;
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    return getClass().getName() + ": " + m_from + " - " + m_to;
  }
  
  /**
   * Creates a <code>DateRangeArgument</code> containing the range:
   * 
   * <pre>[now - pastDays, now]</pre>.
   * 
   * If pastDays == 0, then the range is as follows:
   * 
   * <pre>[ cal.getTime() - now]</pre>
   * 
   * for a Calendar instance which was set like:
   * 
   * <pre>cal.set( 0,0,0,0,0,0 );</pre>
   * 
   * @param pastDays
   * @return new argument
   */
  public static DateRangeArgument createFromPastDays( final int pastDays )
  {
    if( pastDays == 0 )
    {
      final Date d1 = DateUtilities.getMinimum();

      final Date d2 = new Date();

      return new DateRangeArgument( d1, d2 );
    }

    final Calendar cal = Calendar.getInstance();

    final Date d2 = cal.getTime();

    cal.add( Calendar.DAY_OF_YEAR, -pastDays );

    final Date d1 = cal.getTime();

    return new DateRangeArgument( d1, d2 );
  }
}