package org.kalypsodeegree_impl.graphics.sld;

import org.kalypsodeegree.graphics.sld.Interval;

/**
 * 
 * @author N. Peiler
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class Interval_Impl implements Interval, Comparable
{

  private double m_lowerLimit;

  private double m_upperLimit;

  /**
   * constructs an Inteval with the given lower and upper limit
   * 
   * @param lowerLimit
   * @param upperLimit
   */
  public Interval_Impl( double lowerLimit, double upperLimit )
  {
    setLowerLimit( lowerLimit );
    setUpperLimit( upperLimit );
  }

  /**
   * checks if the Interval contains the value x
   * 
   * @param x
   * @return true, if Interval contains the value; otherwise false
   */
  public boolean contains( double x )
  {
    return ( ( m_lowerLimit <= x ) && ( x <= m_upperLimit ) );
  }

  /**
   * @return Returns the lowerLimit.
   */
  public double getLowerLimit()
  {
    return m_lowerLimit;
  }

  /**
   * @return Returns the upperLimit.
   */
  public double getUpperLimit()
  {
    return m_upperLimit;
  }

  /**
   * @param lowerLimit
   *          The lowerLimit to set.
   */
  public void setLowerLimit( double lowerLimit )
  {
    m_lowerLimit = lowerLimit;
  }

  /**
   * @param upperLimit
   *          The upperLimit to set.
   */
  public void setUpperLimit( double upperLimit )
  {
    m_upperLimit = upperLimit;
  }

  public int compareTo( Object o )
  {
    int result = 0;
    double diffLowerLimit = getLowerLimit() - ( (Interval)o ).getLowerLimit();
    if( diffLowerLimit > 0 )
    {
      result = 1;
    }
    if( diffLowerLimit < 0 )
    {
      result = -1;
    }
    return result;
  }
}