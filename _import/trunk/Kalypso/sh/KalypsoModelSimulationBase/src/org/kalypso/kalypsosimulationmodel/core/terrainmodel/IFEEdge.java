/**
 * 
 */
package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

/**
 * Base interface  for all classes abstracting a finite
 * element element in the substitution hierarchy of simBase:_FEEdge 
 * 
 * @author Patrice Congo
 */
public interface IFEEdge<	CT extends IFEElement,
							ET extends IFENode> 
				extends IFeatureWrapper2
{
	public IFeatureWrapperCollection<CT> getContainers();
	public IFeatureWrapperCollection<ET> getNodes();
	
}
