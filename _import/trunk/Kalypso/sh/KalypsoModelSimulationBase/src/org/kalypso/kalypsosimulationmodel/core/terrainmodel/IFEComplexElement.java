package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper;

/**
 * Base tagging interface  for all classes abstracting a finite
 * element element in the substitution hierarchy of simBase:_FENode 
 * 
 * @author Patrice Congo
 */
public interface IFEComplexElement<	CT extends IFEComplexElement, 
									ET extends IFEElement>
				extends IFeatureWrapper
{
	
	
	public  IFeatureWrapperCollection<CT> getContainers();
	
	/**
	 * To get the element this complex element is made of
	 * 
	 * @return a {@link IFeatureWrapperCollection} of element composing
	 * this complex elements
	 */
	public  IFeatureWrapperCollection<ET> getElements();
}
