package org.kalypso.ogc.sensor.beans;

import java.io.Serializable;
import java.util.Date;

/**
 * Simple DateRange Bean. 
 * 
 * @author schlienger
 */
public class DateRangeBean implements Serializable
{
  private long m_from;

  private long m_to;

  public DateRangeBean()
  {
    this( new Date(), new Date() );
  }

  public DateRangeBean( final Date from, final Date to )
  {
    m_from = from.getTime();
    m_to = to.getTime();
  }

  public long getFrom()
  {
    return m_from;
  }

  public void setFrom( long from )
  {
    m_from = from;
  }

  public long getTo()
  {
    return m_to;
  }

  public void setTo( long to )
  {
    m_to = to;
  }
}