package org.kalypso.ogc.sensor.impl;

import org.kalypso.ogc.sensor.IAxisRange;

/**
 * DefaultAxisRange
 * 
 * @author schlienger
 */
public class DefaultAxisRange implements IAxisRange
{
  private final Object m_from;
  private final Object m_to;

  public DefaultAxisRange( final Object from, final Object to )
  {
    m_from = from;
    m_to = to;
  }

  /**
   * @see org.kalypso.ogc.sensor.IAxisRange#getLower()
   */
  public Object getLower( )
  {
    return m_from;
  }

  /**
   * @see org.kalypso.ogc.sensor.IAxisRange#getUpper()
   */
  public Object getUpper( )
  {
    return m_to;
  }
}
