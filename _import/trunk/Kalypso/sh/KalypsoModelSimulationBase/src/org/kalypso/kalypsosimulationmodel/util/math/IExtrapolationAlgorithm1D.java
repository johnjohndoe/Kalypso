/**
 * 
 */
package org.kalypso.kalypsosimulationmodel.util.math;

/**
 * Interface for polynoms interpolation algorithms.
 * 
 * @author Patrice Congo
 *
 */
public interface IExtrapolationAlgorithm1D
{
	/**
	 * Compute the given input a fittin 1D polynom 
	 * 
	 * @param interpolationInput -- the interpolation input
	 * @return the interpolated 1d polynom
	 */
	public IPolynomial1D to1DPolynom(IPolynomialInterpolationInput interpolationInput);
	
}
