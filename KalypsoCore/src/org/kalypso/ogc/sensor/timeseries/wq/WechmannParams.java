package org.kalypso.ogc.sensor.timeseries.wq;

import java.text.NumberFormat;

/**
 * Wechmann Parameters
 * 
 * @author schlienger
 */
public class WechmannParams
{
  /** Konstante W1 */
  private double m_W1;

  /** Konstante LNK1 */
  private double m_LNK1;

  /** Konstante K2 */
  private double m_K2;

  /** obere Wasserstandsgrenze in cm */
  private double m_WGR;

  public WechmannParams()
  {
    this( 0, 0, 0, 0 );
  }

  /**
   * @param W1 Konstante
   * @param LNK1 Konstante
   * @param K2 Konstante
   * @param WGR obere Wasserstandsgrenze in cm
   */
  public WechmannParams( double W1, double LNK1, double K2, double WGR )
  {
    m_W1 = W1;
    m_LNK1 = LNK1;
    m_K2 = K2;
    m_WGR = WGR;
  }

  public double getK2()
  {
    return m_K2;
  }

  public double getLNK1()
  {
    return m_LNK1;
  }

  public double getW1()
  {
    return m_W1;
  }

  public double getWGR()
  {
    return m_WGR;
  }
  
  /**
   * Returns a simple XML-Representation of this object.
   * 
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    final StringBuffer bf = new StringBuffer();
    final NumberFormat nf = NumberFormat.getNumberInstance();

    bf.append( "<params>" );
    
    bf.append( "<w1>" );
    bf.append( nf.format(m_W1) );
    bf.append( "</w1>" );
    
    bf.append( "<lnk1>" );
    bf.append( nf.format(m_LNK1) );
    bf.append( "</lnk1>" );
    
    bf.append( "<k2>" );
    bf.append( nf.format(m_K2) );
    bf.append( "</k2>" );
    
    bf.append( "<wgr>" );
    bf.append( nf.format(m_WGR) );
    bf.append( "</wgr>" );
    
    bf.append( "</params>" );
    
    return bf.toString();
  }
}