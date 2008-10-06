package org.kalypsodeegree_impl.gml.binding.math;

/**
 * Base interface for polynomial classes
 * @author Patrice Congo
 */
public interface IPolynomial
{
	/**
	 * Check the consistency between order and coeffients:
	 * the number of coeficients must be equals to order.
	 *  
	 * @return 
	 */
	public PolynomialConfigState checkConsistency();
}
