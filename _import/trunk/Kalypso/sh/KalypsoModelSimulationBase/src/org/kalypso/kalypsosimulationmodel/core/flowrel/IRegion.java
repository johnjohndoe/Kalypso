/**
 * 
 */
package org.kalypso.kalypsosimulationmodel.core.flowrel;

import org.kalypsodeegree.model.feature.Feature;

public interface IRegion<DomainClass>
{
	public Feature getFeature();
	
	public boolean contains(Feature point);
	public boolean contains(IPosition position);
}