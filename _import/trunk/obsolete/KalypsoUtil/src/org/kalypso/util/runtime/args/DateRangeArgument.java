package org.kalypso.util.runtime.args;

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
}