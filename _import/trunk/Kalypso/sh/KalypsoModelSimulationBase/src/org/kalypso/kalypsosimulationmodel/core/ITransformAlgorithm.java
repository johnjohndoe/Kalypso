/**
 * 
 */
package org.kalypso.kalypsosimulationmodel.core;

/**
 * Implements to provide the algorithm to tranform an
 * Object of a specfic type to another object
 * 
 * @author Dejan Antanaskovic, Patrice Congo
 *
 */
public interface ITransformAlgorithm<ClsSrc,ClsDest> 
{
	/**
	 * 
	 * @param toTransform the object to transform
	 * @return the transformation result
	 */
	public ClsSrc transform(ClsDest toTransform);
	
	
}
