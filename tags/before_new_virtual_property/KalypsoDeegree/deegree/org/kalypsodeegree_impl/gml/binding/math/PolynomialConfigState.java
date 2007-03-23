/**
 * 
 */
package org.kalypsodeegree_impl.gml.binding.math;

public enum PolynomialConfigState {
/**
 * If 1D polynom degree is less than null
 */
NEGATIVE_DEGREE,

/**
 * If degreeX is less than null
 */
NEGATIVE_DEGREEX,

/**
 * If degreeY is than null
 */
NEGATIVE_DEGREEY,
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
CONSISTENCY_OK

}