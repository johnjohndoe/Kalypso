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
  private Date m_from;

  private Date m_to;

  public DateRangeBean()
  {
    this( new Date(), new Date() );
  }

  public DateRangeBean( final Date from, final Date to )
  {
    m_from = from;
    m_to = to;
  }

  public Date getFrom()
  {
    return m_from;
  }

  public void setFrom( Date from )
  {
    m_from = from;
  }

  public Date getTo()
  {
    return m_to;
  }

  public void setTo( Date to )
  {
    m_to = to;
  }
}