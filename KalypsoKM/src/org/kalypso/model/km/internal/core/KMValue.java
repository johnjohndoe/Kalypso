package org.kalypso.model.km.internal.core;

/**
 * Calculates KM values from 2 rows of 1D-simulation results (QW-table of one profile).
 * 
 * @author doemming
 */
class KMValue extends AbstractKMValue
{
  private final double m_qLowerChannel;

  private final double m_qUpperChannel;

  private final double m_qLowerForeland;

  private final double m_qUpperForeland;

  private final double m_alpha;

  private final double m_length;

  private final NKValue m_nkValue;

  private NKValue m_nkValueForeland;

  /**
   * Calculates the Kalinin-Miljukov parameters for one profile (i.e. the coefficients between two selected discharges)
   */
  public KMValue( final double length, final Row row1, final Row row2 )
  {
    m_qLowerChannel = Math.min( row1.getQ(), row2.getQ() );
    m_qUpperChannel = Math.max( row1.getQ(), row2.getQ() );

    m_qLowerForeland = Math.min( row1.getQforeland(), row2.getQforeland() );
    m_qUpperForeland = Math.max( row1.getQforeland(), row2.getQforeland() );

    // mean discharges
    final double qm = (m_qLowerChannel + m_qUpperChannel) / 2d;
    final double qmForeland = (m_qLowerForeland + m_qUpperForeland) / 2d;

    // Delta Watertable
    final double dh = Math.abs( row2.getHNN() - row1.getHNN() );

    // Delta Area River
    final double dA = Math.abs( row2.getArea() - row1.getArea() );

    // Delta Area foreland
    final double dAForeland = Math.abs( row2.getAreaForeland() - row1.getAreaForeland() );

    // Delta discharge River
    final double dq = Math.abs( m_qUpperChannel - m_qLowerChannel );

    // Delta Discharge Forelands
    final double dqForeland = Math.abs( m_qUpperForeland - m_qLowerForeland );

    // Mean Slope
    // TODO: check: a problem occurs, if the slope is negative (first solution: take the absolute values - because
    // negative values are very small)
    final double slope = (Math.abs( row1.getSlope() ) + Math.abs( row2.getSlope() )) / 2d;

    // characteristic length
    final double li = qm * dh / (slope * dq);

    // Retention coefficient River
    // TODO: verify this formula...
    final double ki = (li * dA / dq) / 3600d;

    // Number of storages river
    final double n = length / li;

    m_nkValue = new NKValue( n, ki );

    final double liForeland = qmForeland * dh / (slope * dqForeland);

    if( Double.isInfinite( liForeland ) || Double.isNaN( liForeland ) )
      m_nkValueForeland = new NKValue( 0, 0 );
    else
    {
      final double kiForeland = (liForeland * dAForeland / dqForeland) / 3600d;
      final double nForeland = length / liForeland;
      m_nkValueForeland = new NKValue( nForeland, kiForeland );
    }

    // Distribution factor
    m_alpha = 1 - qmForeland / (qm + qmForeland);
    m_length = length;
  }

  @Override
  public double getAlpha( )
  {
    return m_alpha;
  }

  @Override
  public double getK( )
  {
    return m_nkValue.getK();
  }

  @Override
  public double getN( )
  {
    return m_nkValue.getN();
  }

  @Override
  public double getKForeland( )
  {
    return m_nkValueForeland.getK();
  }

  @Override
  public double getNForeland( )
  {
    return m_nkValueForeland.getN();
  }

  @Override
  public double getLength( )
  {
    return m_length;
  }

  @Override
  public double getLowerQchannel( )
  {
    return m_qLowerChannel;
  }

  @Override
  public double getUpperQchannel( )
  {
    return m_qUpperChannel;
  }

  @Override
  public double getLowerQforeland( )
  {
    return m_qLowerForeland;
  }

  @Override
  public double getUpperQforeland( )
  {
    return m_qUpperForeland;
  }
}