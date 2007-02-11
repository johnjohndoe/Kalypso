package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

/**
 * Base interface  for all classes abstracting a finite
 * element element in the substitution hierarchy of simBase:_FEElement 
 * 
 * @author Patrice Congo
 */public interface IFEElement<	CT extends IFEComplexElement,
 								ET extends IFEEdge> 
 					extends IFeatureWrapper2
{
   // TODO: This binding class correspondg to the gml-type: 'simBase:_FEElement'
   // But '_FEElement' does not contains container or edges
   // why does the interface do?
   // TODO: move getContainers and getEdges to the binding classes where
   // they are first definged in their corresponding gml-types

   public IFeatureWrapperCollection<CT> getContainers();
	 
   public IFeatureWrapperCollection<ET> getEdges();
}
