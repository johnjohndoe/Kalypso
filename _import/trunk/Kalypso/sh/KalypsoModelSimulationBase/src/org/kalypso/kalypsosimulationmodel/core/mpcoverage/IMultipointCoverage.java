package org.kalypso.kalypsosimulationmodel.core.mpcoverage;

import java.util.List;

import org.kalypsodeegree.model.feature.binding.IFeatureWrapper;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * Generic interface to be implemented by classes representing the
 * multipoint coverage for which the range set is a "list" of feature.
 * 
 * All range set feature are required to be adaptable to the template class
 * 
 * 
 * @author Patrice Congo
 */
public interface IMultipointCoverage<RangeCls extends IFeatureWrapper> 
				extends IFeatureWrapper
{
	public IMultiPoint getDomain();
	
	/**
	 * Returns the applying range value  the given position
	 * 
	 * @param location -- position for which a range value are to be found
	 * @return the range value applying at the given position
	 * 
	 * @throws IllegalArgumentException
	 */
	public RangeCls getRangeValue(
						GM_Point location)
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
	public List<RangeCls> getRangeValues(
								GM_Polygon region)
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
	public List<GM_Point> getApplicablePosition(
									RangeCls rangeValue)
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
							RangeCls rangeValue, 
							GM_Point position) 
							throws IllegalArgumentException;
	
	/**
	 * Remove all entry in the coverage concerning the given position
	 * 
	 * @param position
	 * @throws IllegalArgumentException
	 */
	public void removeCoveredPosition(
								GM_Point position) 
								throws IllegalArgumentException;
	/**
	 * 
	 * @param flowRelationship
	 * @param position
	 * @throws IllegalArgumentException if the given flow relationship feature cannot be adapted
	 * 			into an IFlowRelationship 
	 */
	public void removeRangeValue( 
								RangeCls rangeValue) 
								throws IllegalArgumentException;

	
}
