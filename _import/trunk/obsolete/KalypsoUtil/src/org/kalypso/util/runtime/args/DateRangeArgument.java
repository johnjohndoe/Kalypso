package org.kalypso.util.runtime.args;

import java.util.Date;

import org.kalypso.util.runtime.IVariableArguments;


/**
 * Simple Date Range. Can be used as argument where IVariableArguments is required.
 * 
 * @author schlienger
 */
public class DateRangeArgument implements IVariableArguments
{
  private final long m_from;
  private final long m_to;

  public DateRangeArgument( final Date from, final Date to )
  {
    // TODO: Marc: habe die interne Representation nahc long statt Date geändert,
    // um FindBugs zu befriedigen
    // check, obs keine (Performance-)Probleme gibt
    // Gernot
    m_from = from.getTime();
    m_to = to.getTime();
  }

  public Date getFrom()
  {
    return new Date( m_from );
  }
  
  public Date getTo()
  {
    return new Date( m_to );
  }
}
