/**
 * 
 */
package org.kalypso.kalypsosimulationmodel.core.flowrel;

import org.kalypsodeegree.model.feature.Feature;

public interface I3DPosition<DomainClass> extends I2DPosition<DomainClass>
{
	public Feature getFeature();
	public double getZ();
}