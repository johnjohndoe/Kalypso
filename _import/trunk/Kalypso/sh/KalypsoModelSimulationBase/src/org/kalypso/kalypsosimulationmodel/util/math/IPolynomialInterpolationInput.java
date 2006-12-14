package org.kalypso.kalypsosimulationmodel.util.math;

/**
 *
 * @author Patrice Congo
 *
 */
public interface IPolynomialInterpolationInput
{
	/**
	 * Return the dimention of the interpolation 
	 * input. E.g. for a 2D polynms this will be a point cloud 
	 * and the returned is 3. 
	 * 
	 * @return
	 */
	public int getDim();
	
	/**
	 * Returns the interpolation tuples.
	 *  
	 * @return the interpolation tuples as array of arrays with the length 
	 * 	equal to the dimention
	 */
	public double[][] getTupple();	
}
