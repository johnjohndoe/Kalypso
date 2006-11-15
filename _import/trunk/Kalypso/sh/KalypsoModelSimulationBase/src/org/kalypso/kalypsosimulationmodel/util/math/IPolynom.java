
package org.kalypso.kalypsosimulationmodel.util.math;

import org.kalypso.kalypsosimulationmodel.exception.IndexOutOfRangeException;

/**
 * Represents a polynom
 * 
 * @author Patrice Congo
 */
public interface IPolynom
{
	/**
	 * Return the dimension of the polynom
	 * @return the dimention of the polynom as interger
	 */
	public int getDim();
	
	/**
	 * To get the oder of a polynom. 
	 * Depending on the polynom dimention the polynom
	 * may have several order. 
	 * e.g. a 1D dimentional polynom as one order; 
	 * a 2D polynom has 2 (can be view as order in x and
	 * in y direction)
	 *   
	 * @param oderIndex -- the oder index of the polynom
	 * 
	 * @return the order corresponding the given index.
	 */
	public int getOder(int oderIndex) throws IndexOutOfRangeException;
	
	/**
	 * Return all the coeficients of the array as 1 dimentional array.
	 * Note that for polynom with oder equals and greater 2 there is a mapping 
	 * from a to and more dimentional array to a one dimentional array.
	 * For 2 dimentional coefficient aij will be found a index 
	 * i*(getOrder(0))+j in the return array.
	 * 
	 * @return a doouble array containing all the polynom coefficients
	 */
	public double[] getCoefficients();
	
	/**
	 * compute the polynom value for the given value.
	 * E.g.:
	 * <ul>
	 * 	<li/>For a 1D polynom: double[]{1.3};
	 *  <li/>For a 2D polynom: double[]{1.4,3.6}
	 *  <li/> and so on
	 * </ul>
	 * @param input a array of double representing independent value 
	 * @return
	 */
	public double[] comupteResult(double[] input);
}
