
package org.kalypso.kalypsosimulationmodel.util.math;


/**
 * Specifies the interface for object representing a 2D polynom 
 * in the simulation model base schema
 * <pre>
 * </pre>
 * 
 * @author Patrice Congo
 */
public interface IPolynom2D
{
	/**
	 * To get the x order of this polynom. 
	 * @return the x order corresponding the given index.
	 */
	public int getOrderX();
	
	/**
	 * To get the y order of the polynom
	 * @return the y order of the polynom
	 */
	public int getOrderY();
	
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
	 * Sets the polynom parameter, which are its order, 
	 * its dimention and the coefficients
	 *   
	 * @param oder --- the new order auf this polynom
	 * @param dim -- the new dimention of this polynom
	 * @param coefficients -- the coefficient of this polynom
	 * @throws illegal argument exception if 
	 * 	<ul>
	 * 		<li/>orderX or orderY is negative
	 * 		<li/>coefficient is null
	 * 		<li/>number of double passed as coefficient is not
	 * 			equal to order orderX*oderY
	 * 	</ul>
	 * 			
	 */
	public void setPolynomParameters(
								int orderX, 
								int orderY, 
								double[] coefficients);
	
	/**
	 * compute the polynom value for the given value.
	 * 
	 * @param inputX a double for which the x input is to be computed 
	 * @param inputY a double for which the y input is to be computed
	 * @return the polynom value  of the given result.
	 * 
	 */
	public double computeResult(double inputX, double inputY);
}