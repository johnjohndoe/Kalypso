package org.kalypso.kalypsosimulationmodel.core.mtcoverage;

import org.kalypso.kalypsosimulationmodel.core.flowrel.IPosition;
import org.kalypsodeegree.model.feature.Feature;

public class MultipointCoverage<DomainClass> implements IMultipointCoverage<DomainClass>
{
	/**
	 * Add a coverage entry, which is  basically a pair made from a 
	 * a rangeElement feature and the position where it is applying 
	 * @throws IllegalArgumentException if any parameter is null
	 * 
	 */
	public void addCoverageEntry(
						Feature rangeElement, 
						Feature position) 
						throws IllegalArgumentException
	{
		
		
	}

	/**
	 * Get the position where the given flow relationship applyes
	 * @param flowRelationship -- the flow relationship for which the 
	 * 		application locations is are to be computed
	 */
	public IPosition[] getApplicablePosition(
								Feature flowRelationship) 
								throws IllegalArgumentException
	{
		return null;
	}

	public DomainClass getRangeValue(
								Feature location) 
								throws IllegalArgumentException
	{
		return null;
	}

	public DomainClass[] getRangeValues(Feature region) throws IllegalArgumentException
	{
		return null;
	}

	public void removeCoveredPosition(Feature position) throws IllegalArgumentException
	{
		
		
	}

	public void removeRangeValue(Feature flowRelationship) throws IllegalArgumentException
	{
	
		
	}

	
}
