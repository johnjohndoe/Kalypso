package org.kalypso.kalypsosimulationmodel.core.mtcoverage;

import org.kalypso.kalypsosimulationmodel.core.flowrel.IPosition;
import org.kalypsodeegree.model.feature.Feature;

public class MultipointCoverage<DomainClass> implements IMultipointCoverage<DomainClass>
{

	public void addCoverageEntry(Feature flowRelationship, Feature position) throws IllegalArgumentException
	{
		
		
	}

	public IPosition[] getApplicablePosition(Feature flowRelationship) throws IllegalArgumentException
	{
		return null;
	}

	public DomainClass getRangeValue(Feature location) throws IllegalArgumentException
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
