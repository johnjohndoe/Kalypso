
//TODO delelete replace by multipoint generic coverage

package org.kalypso.kalypsosimulationmodel.core.flowrel;

import org.kalypsodeegree.model.feature.Feature;

/**
 * Interface for a flow relationship coverage feature adapter.
 * 
 * @author Patrice Congo
 */
public interface IFlowRelationshipCoverage
{
	
	
	interface IFlowRelationship
	{
		public Feature getFeature();
	}
	
	interface IPosition
	{
		public Feature getFeature();
	}
	
	interface I2DPosition extends IPosition
	{
		public Feature getFeature();
		public double getX();
		public double getY();
	}
	
	interface I3DPosition extends I2DPosition
	{
		public Feature getFeature();
		public double getZ();
	}
	
	interface IRegion
	{
		public Feature getFeature();
		
		public boolean contains(Feature point);
		public boolean contains(IPosition position);
	}
	
	
	/**
	 * Returns the applying flow relatioship at the applying position
	 * 
	 * @param location -- 
	 * @return
	 * @throws IllegalArgumentException
	 */
	public IFlowRelationship getFlowRelationShip(
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
	public IFlowRelationship[] getFlowRelationShips(
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
	public IPosition[] getAppliablePosition(
									Feature flowRelationship)
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
								Feature flowRelationship , 
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
	public void removeFlowRelationship(
								Feature flowRelationship) 
								throws IllegalArgumentException;

	
}
