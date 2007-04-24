package org.kalypso.kalypsosimulationmodel.core.mpcoverage;

import java.util.List;

import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Surface;
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
public interface IMultiSurfaceCoverage<RangeCls extends IFeatureWrapper2> 
			extends IFeatureWrapper2
{
	public IMultiSurface getDomain();
	
	/**
	 * Returns the applying range value for the given position
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
	 * Get all all range value applyable in the given region.
	 * 
	 * @param region the GM_polygon where to find the applyable 
	 * 			range values
	 * @return all flow realtionships applying in this region
	 * @throws IllegalArgumentException if region is null  
	 */
	public List<RangeCls> getRangeValues(
								GM_Polygon region)
								throws IllegalArgumentException;

	/**
	 * To  Get the positions where the given rangeValue applies 
	 * can be applied.
	 * 
	 * @param rangeValue -- the range value wor which the flow relationship feature
	 * @return All positions where this feature can be applied.
	 * 
	 * @throws IllegalArgumentException if passed feature cannot be adapted 
	 * 			to a Iflow Relationship 
	 * 			 
	 */
	public List<GM_Polygon> getApplicablePosition(
									RangeCls rangeValue)
									throws IllegalArgumentException;
	/**
	 * Add a coverage entry at the provided value with a range value
	 * set to the given flow relation ship.
	 *   
	 * @param rangeValue
	 * @param surface
	 * @throws IllegalArgumentException if any parameter is null or
	 * 	 
	 */ 
	public void addCoverageEntry(
							RangeCls rangeValue, 
							GM_Surface surface) 
							throws IllegalArgumentException;
	
	/**
	 * Remove all entry in the coverage concerning the given surface
	 * 
	 * @param surface
	 * @throws IllegalArgumentException
	 */
	public void removeCoveredPosition(
								GM_Surface surface) 
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
