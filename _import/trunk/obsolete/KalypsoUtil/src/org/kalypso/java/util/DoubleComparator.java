package org.kalypso.java.util;

import java.util.Comparator;


/**
 * @author belger
 */
public class DoubleComparator implements Comparator
{
  private double m_delta = 0.0;

  public DoubleComparator( double delta )
  {
    m_delta = delta;
  }

  /**
   * @see java.util.Comparator#compare(Object, Object)
   */
  public int compare( Object o1, Object o2 )
  {
    double d1 = ( (Number)o1 ).doubleValue(  );
    double d2 = ( (Number)o2 ).doubleValue(  );

    return compare( d1, d2 );
  }

  public int compare( double d1, double d2 )
  {
    if( d1 < d2 - m_delta )
      return -1;
    else if( d1 > d2 + m_delta )
      return 1;
    else

      return 0;
  }
}
