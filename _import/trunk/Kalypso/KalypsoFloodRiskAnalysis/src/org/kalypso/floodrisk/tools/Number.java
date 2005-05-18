package org.kalypso.floodrisk.tools;

import java.math.BigDecimal;

/**
 * @author N. Peiler
 *  
 */
public class Number {

	/**
	 * rounds a number
	 * 
	 * @param d
	 *            number
	 * @param scale
	 *            number of internal decimal places
	 * @param mode
	 *            rule for rounding
	 * @see BigDecimal
	 * @return rounded number
	 */
	public static double round(double d, int scale, int mode) {
		BigDecimal bd = new BigDecimal(Double.toString(d));
		return (bd.setScale(scale, mode)).doubleValue();
	}
}