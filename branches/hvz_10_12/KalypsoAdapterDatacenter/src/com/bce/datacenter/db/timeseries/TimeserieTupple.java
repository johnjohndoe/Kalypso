package com.bce.datacenter.db.timeseries;

import java.util.Date;

/**
 * TimeserieTupple
 * 
 * @author marc
 */
public class TimeserieTupple
{
  private Date m_date;
  private Double m_value;
  private String m_status;

  public TimeserieTupple( final Date date, final Double value, final String status )
  {
    m_date = date;
    m_value = value;
    m_status = status;
  }

  /**
   * @return Returns the date.
   */
  public Date getDate( )
  {
    return m_date;
  }
  
  /**
   * @param date The date to set.
   */
  public void setDate( Date date )
  {
    m_date = date;
  }
  
  /**
   * @return Returns the value.
   */
  public Double getValue( )
  {
    return m_value;
  }
  
  /**
   * @param value The value to set.
   */
  public void setValue( Double value )
  {
    m_value = value;
  }
  
  /**
   * @return Returns the status.
   */
  public String getStatus( )
  {
    return m_status;
  }
  
  /**
   * @param status The status to set.
   */
  public void setStatus( String status )
  {
    m_status = status;
  }
}
