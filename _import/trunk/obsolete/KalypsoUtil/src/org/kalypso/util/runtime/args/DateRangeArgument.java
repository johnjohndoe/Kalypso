package org.kalypso.util.runtime.args;

import java.util.Calendar;
import java.util.Date;

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

  public DateRangeArgument( final Date from, final Date to )
  {
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
   */
  public static DateRangeArgument createFromPastDays( final int pastDays )
  {
    if( pastDays == 0 )
    {
      final Calendar cal = Calendar.getInstance();
      cal.set( 0, 0, 0, 0, 0, 0 );

      final Date d1 = cal.getTime();

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