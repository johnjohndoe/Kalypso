package org.kalypso.model.km.internal.core;

/*
 * Eine Reihe der KM-Dateien
 */
class Row
{
  private final double m_hNN;

  private final double m_q;

  private final double m_qforeland;

  private final double m_a;

  private final double m_af;

  private final double m_w;

  private final double m_wf;

  private final double m_i;

  public Row( final double hNN, final double q, final double qf, final double a, final double af, final double w, final double wf, final double i )
  {
    m_hNN = hNN;
    m_q = q;
    m_qforeland = qf;
    m_a = a;
    m_af = af;
    m_w = w;
    m_wf = wf;
    m_i = i;

  }

  public double getH( )
  {

    return m_hNN;
  }

  @Override
  public String toString( )
  {
    return "" + m_hNN + " " + m_q + " " + m_qforeland + " " + m_a + " " //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
    + m_af + " " + m_w + " " + m_wf + " " + m_i; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  }

  public double getQ( )
  {
    return m_q;
  }

  public double getHNN( )
  {
    return m_hNN;
  }

  public double getQforeland( )
  {
    return m_qforeland;
  }

  public double getSlope( )
  {
    return m_i;
  }

  public double getWidth( )
  {

    return m_w;
  }

  public double getWidthForeland( )
  {
    return m_wf;
  }

  public double getArea( )
  {
    return m_a;
  }

  public double getAreaForeland( )
  {
    return m_af;
  }

  public double getAlpha( )
  {
    return m_q / getQfull();
  }

  private double getQfull( )
  {
    return m_q + m_qforeland;
  }

}