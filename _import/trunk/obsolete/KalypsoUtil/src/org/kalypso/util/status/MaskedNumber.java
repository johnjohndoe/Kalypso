package org.kalypso.util.status;

/**
 * A Number with a Bit-Mask.
 * 
 * @author schlienger
 */
public class MaskedNumber
{
  private final int m_mask;

  private final Number m_number;

  public MaskedNumber( final double value )
  {
    this( new Double( value ), 0 );
  }
  
  public MaskedNumber( final Number number, final int mask )
  {
    m_number = number;
    m_mask = mask;
  }

  public int getMask()
  {
    return m_mask;
  }

  public Number getNumber()
  {
    return m_number;
  }

  /**
   * Tests if the given mask is part of this
   * 
   * @param mask
   *          some BitMask to test
   */
  public boolean contains( final int mask )
  {
    return ( m_mask & mask ) == mask;
  }

  /**
   * @see MaskedNumber#contains(int)
   */
  public boolean contains( final MaskedNumber mn )
  {
    return contains( mn.m_mask );
  }

  /**
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    return m_number.toString() + " (Mask: " + String.valueOf( m_mask ) + ')';
  }

  public byte byteValue()
  {
    return m_number.byteValue();
  }

  public double doubleValue()
  {
    return m_number.doubleValue();
  }

  /**
   * TODO: auch m_mask berücksichtigen?
   * 
   * @see java.lang.Object#equals(java.lang.Object)
   */
  public boolean equals( Object obj )
  {
    return m_number.equals( obj );
  }

  public float floatValue()
  {
    return m_number.floatValue();
  }

  /**
   * TODO: auch m_mask berücksichtigen?
   * 
   * @see java.lang.Object#hashCode()
   */
  public int hashCode()
  {
    return m_number.hashCode();
  }

  public int intValue()
  {
    return m_number.intValue();
  }

  public long longValue()
  {
    return m_number.longValue();
  }

  public short shortValue()
  {
    return m_number.shortValue();
  }
}