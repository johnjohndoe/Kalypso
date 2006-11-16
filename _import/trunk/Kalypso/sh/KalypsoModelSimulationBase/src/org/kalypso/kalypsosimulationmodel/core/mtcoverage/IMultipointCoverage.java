package org.kalypso.kalypsosimulationmodel.core.mtcoverage;

import org.kalypso.kalypsosimulationmodel.core.flowrel.IPosition;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IRegion;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Interface for a flow relationship coverage feature adapter.
 * 
 * @author Patrice Congo
 */
public interface IMultipointCoverage<RangeClass>
{
	
	/**
	 * Returns the applying range value  the given position
	 * 
	 * @param location -- position for which a range value are to be found
	 * @return the range value applying at the given position
	 * 
	 * @throws IllegalArgumentException
	 */
	public RangeClass getRangeValue(
									Feature location)
									throws IllegalArgumentException;
	
	
	/**
	 * Get all flow relationhip applying in the given region.
	 * The region is pass as feature but must be adaptable an IRegion
	 * 
	 * @param region
	 * @return all flow realtionships applying in this region
	 * @throws IllegalArgumentException if region is null or cannot be
	 * 			adapted into a {@link IRegion} 
	 */
	public RangeClass[] getRangeValues(
										Feature region)
										throws IllegalArgumentException;

	/**
	 * To  Get the position where the given flow relationship can be applied.
	 * 
	 * @param flowRelationship -- a flow relationship feature
	 * @return All positions where this feature can be applied.
	 * 
	 * @throws IllegalArgumentException if passed feature cannot be adapted 
	 * 			to a Iflow Relationship 
	 * 			 
	 */
	public IPosition[] getApplicablePosition(
									Feature rangeValue)
									throws IllegalArgumentException;
	/**
	 * Add a coverage entry at the provided value with a range value
	 * set to the given flow relation ship.
	 *   
	 * @param flowRelationship
	 * @param location
	 * @throws IllegalArgumentException if any parameter is null or
	 * 	flowRelationship cannot be adapted to a flowrelationship or 
	 *  or position feature cannot be adapted into a IPosition 
	 */
	public void addCoverageEntry(
								Feature rangeValue, 
								Feature position) 
								throws IllegalArgumentException;
	
	/**
	 * Remove all entry in the coverage concerning the given position
	 * 
	 * @param position
	 * @throws IllegalArgumentException
	 */
	public void removeCoveredPosition(
								Feature position) 
								throws IllegalArgumentException;
	/**
	 * 
	 * @param flowRelationship
	 * @param position
	 * @throws IllegalArgumentException if the given flow relationship feature cannot be adapted
	 * 			into an IFlowRelationship 
	 */
	public void removeRangeValue(
								Feature rangeValue) 
								throws IllegalArgumentException;

	
}
