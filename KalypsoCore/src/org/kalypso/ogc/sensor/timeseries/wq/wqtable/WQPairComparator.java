package org.kalypso.ogc.sensor.timeseries.wq.wqtable;

import java.util.Comparator;

/**
 * WQPairComparator
 * 
 * @author schlienger
 */
public class WQPairComparator implements Comparator<WQPair>
{
  public static final int W = 0;

  public static final int Q = 1;

  public static final WQPairComparator Q_COMPARATOR = new WQPairComparator( Q );

  public static final WQPairComparator W_COMPARATOR = new WQPairComparator( W );

  private final int m_field;

  private WQPairComparator( final int field )
  {
    m_field = field;
  }

  /**
   * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
   */
  public int compare( final WQPair p1, final WQPair p2 )
  {
    if( m_field == W )
      return Double.compare( p1.getW(), p2.getW() );

    return Double.compare( p1.getQ(), p2.getQ() );
  }
}
