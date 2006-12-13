
package org.kalypso.kalypsosimulationmodel.util.math;


/**
 * Specifies the interface for object representing a 1D polynom 
 * in the simulation model base schema
 * <pre>
 * &ltxs:complexType 
 *		name="Polynomial1DType"
 *		abstract="false">
 *		&ltxs:annotation>
 *			&ltxs:documentation&gt
 *				Type for 1D polynoms: y=sum of ai*xi for all i less than oder
 *			&lt/xs:documentation&gt
 *		&lt/xs:annotation&gt
 *		&ltxs:complexContent&gt
 *			&ltxs:extension 
 *				base="simBase:AbstractPolynomialType"&gt
 *				&ltxs:sequence&gt
 *					&ltxs:element 
 *						name="order"
 *						type="xs:positiveInteger"
 *						minOccurs="1" maxOccurs="1"/&gt
 *					&ltxs:element
 *						name="coefficients"
 *						type="gml:doubleOrNullList"/&gt
 *				&lt/xs:sequence&gt
 *			&lt/xs:extension&gt
 *		&lt/xs:complexContent&gt
 *	&lt/xs:complexType&gt
 * </pre>
 * 
 * @author Patrice Congo
 */
public interface IPolynom1D
{
//	/**
//	 * Return the dimension of the polynom
//	 * @return the dimention of the polynom as interger
//	 */
//	public int getDim();
	
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
	public int getOrder();
	
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
	 * 		<li/>order or is negative
	 * 		<li/>coefficient is null
	 * 		<li/>number of double passed as coefficient is not
	 * 			equal to order
	 * 	</ul>
	 * 			
	 */
	public void setPolynomParameters(int order, double[] coefficients);
	
	/**
	 * compute the polynom value for the given value.
	 * 
	 * @param input a double for which the input is to be computed 
	 * @return the polynom value  of the given result.
	 * 
	 */
	public double computeResult(double input);
	
	
	
}
