package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.util.List;

import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;

/**
 * Interface to be implemented by class which represents the
 * simBase:RiverProfileNetworkCollection
 * 
 * @author Patrice Congo
 *
 */
public interface IRiverProfileNetworkCollection 
				extends IFeatureWrapperCollection<IRiverProfileNetwork>
{
	/**
	 * Select river profile network base which names matches the given
	 * regular expression 
	 *  
	 * @param regExp the regular expressen to match with
	 * 
	 * @return a {@link List} of river profile networks that matches the
	 * 			the given regular expression
	 */
	public List<IRiverProfileNetwork> selectRiverProfileNetwork(String regExp);
}
