package org.kalypso.util.status;

/**
 * BitMask.
 * 
 * @author schlienger
 */
public class BitMask
{
  private final int m_mask;
  
  public BitMask( final int mask )
  {
    m_mask = mask;
  }
  
  /**
   * Tests if the given mask is part of this
   * @param mask some BitMask to test
   */
  public boolean test( final int mask )
  {
    return (m_mask & mask) == mask;
  }
  
  /**
   * @see BitMask#test(int)
   */
  public boolean test( final BitMask mask )
  {
    return test( mask.m_mask );
  }
  
  /**
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    return String.valueOf( m_mask );
  }
}
