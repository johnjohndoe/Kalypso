package org.kalypso.kalypsosimulationmodel.util.math;

/**
 *
 * @author Patrice Congo
 *
 */
public interface IPolynomInterpolationInput
{
	/**
	 * Return the dimention of the interpolation 
	 * input. E.g. for a 2D polynms this will be a point cloud in the 3D space 
	 * and the returned is 4. 
	 * 
	 * @return
	 */
	public int getDim();
	
	/**
	 * Returns the interpolation tuples.
	 *  
	 * @return the interpolation tuples
	 */
	public double[][] getTupple();	
}
