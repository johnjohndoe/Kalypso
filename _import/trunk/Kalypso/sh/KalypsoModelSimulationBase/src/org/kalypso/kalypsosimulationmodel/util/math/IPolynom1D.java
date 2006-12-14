
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
	static public enum CONSISTENCY_CHECK {
		/**
		 * If less or equals to null
		 */
		ILLEGAL_ORGER, 
		/**
		 * If order not equals to the number of coefficients
		 */
		ORDER_COEF_MISMATCH,
		
		/**
		 * Basically the number of the coefficients is equals to the
		 * order+1; and the last coeficient must be non null. 
		 * Use this value to signal  that the most significant 
		 * coefficient is zero  
		 */
		ZERO_MOST_SIGNIFICANT_COEFS,
		/**
		 * The cofiguration set with order and the coefficients is
		 * consistent
		 */
		CONSISTENCY_OK};
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
	 * Set order without any relational consistancy check.
	 * One will typically call {}
	 * 
	 * @param order
	 * @throws IllegalArgumentException if the passed order is 
	 * 			equal or less then zero
	 */
	public void setOrder(
					int order) 
					throws IllegalArgumentException;
	
	/**
	 * Return all the coeficients of the array as 1 dimentional array.
	 * 
	 * @return a double array containing all the polynom coefficients
	 */
	public double[] getCoefficients();
	
	/**
	 * Set coefficients without a realtional check with the
	 * already set coefficient.
	 * 
	 * @param coefficients the coefficients to set
	 * @throws IllegalArgumentException if coefficients is null or
	 * 	the last element in coefficient is zero 
	 * (since this has an implication when checking the order of the 
	 * polynom).
	 * 
	 */
	public void setCoefficients(
							double[] coefficients)
							throws IllegalArgumentException;
	
	/**
	 * Sets the polynom parameter, which are its order, 
	 * its dimention and the coefficients
	 *   
	 * @param oder --- the new order auf this polynom
	 * @param coefficients -- the coefficient of this polynom
	 * @throws illegal argument exception if 
	 * 	<ul>
	 * 		<li/>order zero or is negative
	 * 		<li/>coefficient is null
	 * 		<li/>number of double passed as coefficient is not
	 * 			equal to order+1
	 * 	</ul>
	 * 			
	 */
	public void setPolynomParameters(
								int order, 
								double[] coefficients)
								throws IllegalArgumentException;
								
	/**
	 * compute the polynom value for the given value.
	 * 
	 * @param input a double for which the input is to be computed 
	 * @return the polynom value  of the given result.
	 * 
	 */
	public double computeResult(double input);
	
	/**
	 * Check the consistency between order and coeffients:
	 * the number of coeficients must be equals to order.
	 *  
	 * @return 
	 */
	public CONSISTENCY_CHECK checkConsistency();
	
}
