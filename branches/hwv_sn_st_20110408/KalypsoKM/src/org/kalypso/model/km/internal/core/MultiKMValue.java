package org.kalypso.model.km.internal.core;


/**
 * Aggregation of Kalinin-Miljukov parameters for one river strand.
 * 
 * @author doemming
 */
class MultiKMValue extends AbstractKMValue
{
  /* Average (weighted by length) of all inner alphas's */
  private final double m_alpha;

  /* Sum of all inner km-length */
  private final double m_length;

  /* Average (weighted by length) of all inner k's */
  private final double m_k;

  /* Average (weighted by length) of all inner kForeland's */
  private final double m_kForeland;

  /* Sum of all inner n's */
  private final double m_n;

  /* Sum of all inner nForeland's */
  private final double m_nForeland;

  /* q of first km */
  private final double m_q;

  /* qForeland of first km */
  private final double m_qForeland;

  public MultiKMValue( final IKMValue[] values )
  {
    m_q = values[0].getQ();
    m_qForeland = values[0].getQForeland();

    double length = 0;
    double alpha = 0;
    double k = 0;
    double kForeland = 0;
    double n = 0;
    double nForeland = 0;
    for( final IKMValue km : values )
    {
      length += km.getLength();
      alpha += km.getAlpha() * km.getLength();
      k += km.getK() * km.getLength();
      kForeland += km.getKForeland() * km.getLength();
      n += km.getN();
      nForeland += km.getNForeland();
    }

    // calculate mean values for the strand by the merged profiles
    m_alpha = alpha / length;
    m_length = length;
    m_k = k / length;
    m_kForeland = kForeland / length;
    m_n = n;
    m_nForeland = nForeland;
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
    return m_qForeland;
  }

}
