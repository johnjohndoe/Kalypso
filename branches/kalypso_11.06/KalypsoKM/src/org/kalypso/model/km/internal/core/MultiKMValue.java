package org.kalypso.model.km.internal.core;


/**
 * Aggregation of Kalinin-Miljukov parameters for one river strand.
 * 
 * @author doemming
 */
class MultiKMValue extends AbstractKMValue
{
  private final double m_qLowerChannel;

  private final double m_qUpperChannel;

  private final double m_qLowerForeland;

  private final double m_qUpperForeland;

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

  public MultiKMValue( final IKMValue[] values )
  {
    m_qLowerChannel = values[0].getLowerQchannel();
    m_qUpperChannel = values[0].getUpperQchannel();

    m_qLowerForeland = values[0].getLowerQforeland();
    m_qUpperForeland = values[0].getUpperQforeland();

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