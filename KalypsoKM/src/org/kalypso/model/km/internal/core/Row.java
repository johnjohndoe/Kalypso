package org.kalypso.model.km.internal.core;

/**
 * Eine Reihe der KM-Dateien.
 */
public class Row
{
  /**
   * The heigth of the waterlevel.
   */
  private double m_hNN;

  /**
   * The runoff of the main channel.
   */
  private double m_q;

  /**
   * The runoff of the foreland.
   */
  private double m_qf;

  /**
   * The area of the main channel.
   */
  private double m_a;

  /**
   * The area of the foreland.
   */
  private double m_af;

  /**
   * The width of the main channel.
   */
  private double m_w;

  /**
   * The width of the foreland.
   */
  private double m_wf;

  /**
   * The slope.
   */
  private double m_i;

  public Row( final double hNN, final double q, final double qf, final double a, final double af, final double w, final double wf, final double i )
  {
    m_hNN = hNN;
    m_q = q;
    m_qf = qf;
    m_a = a;
    m_af = af;
    m_w = w;
    m_wf = wf;
    m_i = i;
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    return "" + m_hNN + " " + m_q + " " + m_qf + " " + m_a + " " //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
        + m_af + " " + m_w + " " + m_wf + " " + m_i; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  }

  public double getHNN( )
  {
    return m_hNN;
  }

  public double getQ( )
  {
    return m_q;
  }

  public double getQforeland( )
  {
    return m_qf;
  }

  public double getArea( )
  {
    return m_a;
  }

  public double getAreaForeland( )
  {
    return m_af;
  }

  public double getWidth( )
  {

    return m_w;
  }

  public double getWidthForeland( )
  {
    return m_wf;
  }

  public double getSlope( )
  {
    return m_i;
  }

  public double getAlpha( )
  {
    return m_q / getQfull();
  }

  private double getQfull( )
  {
    return m_q + m_qf;
  }
}