package org.kalypso.ogc.sensor.timeseries.wq;

import java.text.NumberFormat;

/**
 * Wechmann Parameters
 * 
 * @author schlienger
 */
public final class WechmannParams
{
  /** Konstante W1 */
  private final double m_W1;

  /** Konstante LNK1 */
  private final double m_LNK1;

  /** Konstante K2 */
  private final double m_K2;

  /**
   * obere Wasserstandsgrenze in cm. When no WGR value is defined, this class
   * supposes Double.MAX_VALUE is used
   */
  private final double m_WGR;
  
  /**
   * this is the Q computed from WGR usign the Wechmann function. This Q is also
   * stored as a member of this class because it is used when convert Q to W.
   * <p>
   * this is a computed value, it is not serialized.
   */
  private final double m_Q4WGR;

  // used for persistence
  private static final String TAG_W1 = "w1";
  private static final String TAG_LNK1 = "lnk1";
  private static final String TAG_K2 = "k2";
  private static final String TAG_WGR = "wgr";

  /**
   * Creates the parameters with a WGR value of Double.MAX_VALUE. Use this
   * constructor when the WGR value is not defined, thus the parameters are
   * valid for all possible W values.
   * 
   * @throws WechmannException
   */
  public WechmannParams( double W1, double LNK1, double K2 ) throws WechmannException
  {
    this( W1, LNK1, K2, Double.MAX_VALUE );
  }

  /**
   * Creates the parameters. They will be used until the Waterlevel reaches WGR.
   * 
   * @param W1
   *          Konstante
   * @param LNK1
   *          Konstante
   * @param K2
   *          Konstante
   * @param WGR
   *          obere Wasserstandsgrenze in cm
   * 
   * @throws WechmannException
   */
  public WechmannParams( double W1, double LNK1, double K2, double WGR ) throws WechmannException
  {
    m_W1 = W1;
    m_LNK1 = LNK1;
    m_K2 = K2;
    m_WGR = WGR;
    
    m_Q4WGR = WechmannFunction.computeQ( LNK1, WGR, W1, K2 );
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
  
  public double getQ4WGR()
  {
    return m_Q4WGR;
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
    nf.setMinimumFractionDigits( 5 );

    bf.append( "<" ).append( TAG_W1 ).append( ">" );
    bf.append( nf.format( m_W1 ) );
    bf.append( "</" ).append( TAG_W1 ).append( ">" );

    bf.append( "<" ).append( TAG_LNK1 ).append( ">" );
    bf.append( nf.format( m_LNK1 ) );
    bf.append( "</" ).append( TAG_LNK1 ).append( ">" );

    bf.append( "<" ).append( TAG_K2 ).append( ">" );
    bf.append( nf.format( m_K2 ) );
    bf.append( "</" ).append( TAG_K2 ).append( ">" );

    // only include the WGR value if it is defined
    if( Double.compare( m_WGR, Double.MAX_VALUE ) != 0 )
    {
      bf.append( "<" ).append( TAG_WGR ).append( ">" );
      bf.append( nf.format( m_WGR ) );
      bf.append( "</" ).append( TAG_WGR ).append( ">" );
    }

    return bf.toString();
  }
}