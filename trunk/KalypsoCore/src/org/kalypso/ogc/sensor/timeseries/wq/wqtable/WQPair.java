package org.kalypso.ogc.sensor.timeseries.wq.wqtable;

/**
 * WQPair
 * 
 * @author schlienger
 */
public final class WQPair
{
  private final double m_w;
  private final double m_q;

  public WQPair( final double W, final double Q )
  {
    m_w = W;
    m_q = Q;
  }

  public double getQ( )
  {
    return m_q;
  }
  
  public double getW()
  {
    return m_w;
  }
  
  public String toString( )
  {
    return "W= " + m_w + " Q= " + m_q;
  }
}
