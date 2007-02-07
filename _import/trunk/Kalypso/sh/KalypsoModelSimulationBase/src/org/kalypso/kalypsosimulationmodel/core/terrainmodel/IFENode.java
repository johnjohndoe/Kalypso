package org.kalypso.kalypsosimulationmodel.core.terrainmodel;


import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper;

/**
 * Base tagging interface  for all classes abstracting a finite
 * element element in the substitution hierarchy of simBase:_FENode 
 * 
 * @author Patrice Congo
 */
public interface IFENode <CT extends IFEEdge> extends IFeatureWrapper
{
	//empty
	public  IFeatureWrapperCollection<CT> getContainers();
}
