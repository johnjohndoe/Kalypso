package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.util.List;

import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapper;

/**
 * Base interface  for all classes abstracting a finite
 * element element in the substitution hierarchy of simBase:_FEElement 
 * 
 * @author Patrice Congo
 */public interface IFEElement<	CT extends IFEComplexElement,
 								ET extends IFEEdge> 
 					extends IFeatureWrapper
{
	 public List<CT> getContainers();
	 
	 public List<ET> getEdges();
}
