/**
 * 
 */
package org.kalypso.kalypsosimulationmodel.core.flowrel;

import org.kalypsodeegree.model.feature.Feature;

public interface I2DPosition<DomainClass> extends IPosition
{
	public Feature getFeature();
	public double getX();
	public double getY();
}