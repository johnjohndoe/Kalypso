package org.kalypsodeegree_impl.gml.binding.math;



public interface IInterpolationAlgorithm<T extends IPolynomial>
{
	
	public T toPolynomial(IPolynomialInterpolationInput interpolationInput);
	
}
