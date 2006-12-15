package org.kalypso.kalypsosimulationmodel.util.math;



public interface IInterpolationAlgorithm<T extends IPolynomial>
{
	
	public T toPolynomial(IPolynomialInterpolationInput interpolationInput);
	
}
