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

  public double getQ()
  {
    return m_q;
  }

  public double getW()
  {
    return m_w;
  }

  @Override
  public String toString( )
  {
    return "W= " + m_w + " Q= " + m_q;
  }

  public static void convert2doubles( final WQPair[] pairs, final double[] W, final double[] Q )
  {
    for( int i = 0; i < pairs.length; i++ )
    {
      W[i] = pairs[i].getW();
      Q[i] = pairs[i].getQ();
    }
  }

  public static WQPair[] convert2pairs( final double[][] table )
  {
    final WQPair[] pairs = new WQPair[table.length];
    for( int i = 0; i < table.length; i++ )
      pairs[i] = new WQPair( table[i][0], table[i][1] );

    return pairs;
  }

  public static WQPair[] convert2pairs( final double[] W, final double[] Q )
  {
    if( W.length != Q.length )
      throw new IllegalArgumentException( "Anzahl von Ws und Qs ist nicht gleich" );

    final WQPair[] pairs = new WQPair[W.length];
    for( int i = 0; i < W.length; i++ )
      pairs[i] = new WQPair( W[i], Q[i] );

    return pairs;
  }

  public static WQPair[] convert2pairs( final Number[] W, final Number[] Q )
  {
    if( W.length != Q.length )
      throw new IllegalArgumentException( "Anzahl von Ws und Qs ist nicht gleich" );

    final WQPair[] pairs = new WQPair[W.length];
    for( int i = 0; i < W.length; i++ )
      pairs[i] = new WQPair( W[i].doubleValue(), Q[i].doubleValue() );

    return pairs;
  }
}
