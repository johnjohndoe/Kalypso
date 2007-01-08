package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;

/**
 * Interface to be implemented by classes that represent a 
 * simBase:RiverProfileNetwork
 * 
 * @author Patrice Congo
 */
public interface IRiverProfileNetwork 
			extends IFeatureWrapperCollection<IRiverProfile>
{
	/**
	 * To get the river profile which is located before the given river
	 *  
	 * @param riverProfile the reference profile
	 * @return the  river profile situated before the specified one
	 */
	public IRiverProfile getPrevious(IRiverProfile riverProfile);
	
	/**
	 * To get the river profile which is located after the given river
	 *  
	 * @param riverProfile the reference profile
	 * @return the  river profile situated after the specified one
	 */
	public IRiverProfile getNext(IRiverProfile riverProfile);
	
}
