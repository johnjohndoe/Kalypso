package org.kalypsodeegree_impl.gml.binding.math;

/**
 *
 * @author Patrice Congo
 *
 */
public interface IPolynomialInterpolationInput
{
	/**
	 * Return the dimention of the interpolation 
	 * input. E.g. for a 2D polynms this will be a point cloud in 3D-Space 
	 * and the returned is 3. 
	 * 
	 * @return
	 */
	public int getDim();
	
	/**
	 * Returns the degree for the parameter with the given rank.
	 * Valid value of the rank are element of [0,getDim()-1]
	 * 
	 * @param dimRank
	 * @return Returns the degree for the parameter with the given rank
	 */
	public int getDegree(int dimRank);
	
	/**
	 * Returns the interpolation tuples.
	 *  
	 * @return the interpolation tuples as array of arrays with the length 
	 * 	equal to the dimention
	 */
	public double[][] getTupple();	
}
