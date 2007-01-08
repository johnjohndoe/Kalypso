package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.util.List;

import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapper;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;

/**
 * Base interface  for all classes abstracting a finite
 * element element in the substitution hierarchy of simBase:_FEElement 
 * 
 * @author Patrice Congo
 */public interface IFEElement<	CT extends IFEComplexElement,
 								ET extends IFEEdge> 
 					extends IFeatureWrapper
{
	 public IFeatureWrapperCollection<CT> getContainers();
	 
	 public IFeatureWrapperCollection<ET> getEdges();
}
