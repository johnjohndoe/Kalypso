package com.bce.eind.core.profil.util.geom;

/**
 * Represents a geometrical Polygone (points are stored in double format)
 *
 * @author schlienger
 */
public class PolyGone
{
  private double[] m_x;
  private double[] m_y;

  /**
   * Default constructor, members are set to empty array.
   */
  public PolyGone(  )
  {
    m_x = new double[0];
    m_y = m_x;
  }

  /**
   * constructor, points must be sorted (arbitrary).
   *
   */
  public PolyGone( double[] x, double[] y )
  {
    m_x = x;
    m_y = y;
  }

  /**
   * adds points from the PolyLine after members (order is specified by bInvert)
   *
   * @param line
   * @param bInvert when true, takes the points from line in the opposite direction (on X)
   */
  public void addPoints( PolyLine line, boolean bInvert )
  {
    addPoints( line.getX(  ), line.getY(  ), bInvert );
  }

  /**
   * adds points after members (order is specified by bInvert)
   *
   * @param x
   * @param y 
   * @param bInvert when true, takes the points from line in the opposite direction (on X)
   */
  public void addPoints( double[] x, double[] y, boolean bInvert )
  {
    double[] newX = new double[m_x.length + x.length];
    double[] newY = new double[newX.length];

    // copy members
    System.arraycopy( m_x, 0, newX, 0, m_x.length );
    System.arraycopy( m_y, 0, newY, 0, m_y.length );

    /*
     * either invert the order or not depending on bInvert.
     */
    if( bInvert )
    {
      // copy line ones, but beginning from the end
      for( int i = x.length - 1; i >= 0; i-- )
      {
        int pos = newX.length - i - 1;
        newX[pos] = x[i];
        newY[pos] = y[i];
      }
    }
    else
    {
      // copy line ones
      System.arraycopy( x, 0, newX, m_x.length, x.length );
      System.arraycopy( y, 0, newY, m_y.length, y.length );
    }

    m_x = newX;
    m_y = newY;
  }

  /**
   * -
   *
   * @return -
   */
  public double[] getX(  )
  {
    return m_x;
  }

  /**
   * -
   *
   * @return -
   */
  public double[] getY(  )
  {
    return m_y;
  }

  /**
   * -
   *
   * @return -
   */
  public String toString(  )
  {
    StringBuffer b = new StringBuffer(  );

    for( int i = 0; i < m_x.length; i++ )
      b.append( m_x[i] ).append( " - " ).append( m_y[i] ).append( "\n" );

    return b.toString(  );
  }
}
