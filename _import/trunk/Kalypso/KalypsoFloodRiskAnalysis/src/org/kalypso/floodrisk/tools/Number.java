package org.kalypso.floodrisk.tools;

import java.math.BigDecimal;

/**
 * 
 * Number
 * <p>
 * 
 * created by
 * 
 * @author Nadja Peiler (17.06.2005)
 */
public class Number
{

  /**
   * round a number "d" with "scale" decimal places considering the given "mode"
   * (BidDecimal.class)
   * 
   * @param d number to round
   * @param scale number of decimal places
   * @param mode rule for rounding
   * @see BigDecimal
   * @return rounded number
   */
  public static double round( double d, int scale, int mode )
  {
    BigDecimal bd = new BigDecimal( Double.toString( d ) );
    return ( bd.setScale( scale, mode ) ).doubleValue();
  }
}