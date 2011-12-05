package org.kalypso.model.km.internal.core;

/**
 * Calculates KM values from 2 rows of 1D-simulation results (QW-table of one profile).
 * 
 * @author doemming
 */
class KMValue extends AbstractKMValue
{
  private final double m_k;

  private final double m_n;

  private final double m_kForeland;

  private final double m_nForeland;

  private final double m_alpha;

  private final double m_length;

  private final double m_q;

  private final double m_qf;

  /*
   * Calculates the Kalinin-Miljukov parameters for one profile (one set per discharge)
   */
  public KMValue( final double length, final Row row1, final Row row2 )
  {
    // Discharge River
    final double qm = (row1.getQ() + row2.getQ()) / 2d;

    // Discharge Forelands
    final double qmForeland = (row1.getQforeland() + row2.getQforeland()) / 2d;

    // Delta Watertable
    final double dh = Math.abs( row2.getHNN() - row1.getHNN() );

    // Delta Area River
    final double dA = Math.abs( row2.getArea() - row1.getArea() );

    // Delta Area Forelands
    final double dAForeland = Math.abs( row2.getAreaForeland() - row1.getAreaForeland() );

    // Delta Dischrage River
    final double dq = Math.abs( row2.getQ() - row1.getQ() );

    // Delta Discharge Forelands
    final double dqForeland = Math.abs( row2.getQforeland() - row1.getQforeland() );
    // Mean Slope
    // TODO: check: a problem occures, if the slope is negative (first solution: take the absoulute values - because
    // negative values
    // are very small)
    final double slope = (Math.abs( row1.getSlope() ) + Math.abs( row2.getSlope() )) / 2d;

    final double li = qm * dh / (slope * dq);

    // Retention coefficient River
    final double ki = (li * dA / dq) / 3600d;

    // Number of storages river
    final double n = length / li;

    final double liForeland = qmForeland * dh / (slope * dqForeland);
    if( Double.isInfinite( liForeland ) || Double.isNaN( liForeland ) )
    {
      m_kForeland = 0;
      m_nForeland = 0;
    }
    else
    {
      // Retention coefficient forlands
      final double kiForeland = (liForeland * dAForeland / dqForeland) / 3600d;
      // Number of storages forelands
      final double nForeland = length / liForeland;

      m_kForeland = kiForeland;
      m_nForeland = nForeland;
    }

    // Distribution factor
    m_alpha = 1 - qmForeland / (qm + qmForeland);
    m_length = length;
    m_q = qm;
    m_qf = qmForeland;
    m_k = ki;
    m_n = n;
  }

  @Override
  public double getAlpha( )
  {
    return m_alpha;
  }

  @Override
  public double getKForeland( )
  {
    return m_kForeland;
  }

  @Override
  public double getNForeland( )
  {
    return m_nForeland;
  }

  @Override
  public double getK( )
  {
    return m_k;
  }

  @Override
  public double getN( )
  {
    return m_n;
  }

  @Override
  public double getLength( )
  {
    return m_length;
  }

  @Override
  public double getQ( )
  {
    return m_q;
  }

  @Override
  public double getQForeland( )
  {
    return m_qf;
  }

}
