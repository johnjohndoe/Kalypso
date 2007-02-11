package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

/**
 * Base tagging interface for all classes abstracting a finite element element in the substitution hierarchy of
 * simBase:_FENode
 * 
 * @author Patrice Congo
 */
public interface IFEComplexElement<CT extends IFEComplexElement, ET extends IFEElement> extends IFeatureWrapper2
{
  // TODO: the corresponding feature-type has no 'containers'
  // so remove this here
  public IFeatureWrapperCollection<CT> getContainers( );

  /**
   * To get the element this complex element is made of
   * 
   * @return a {@link IFeatureWrapperCollection} of element composing this complex elements
   */
  // TODO: the corresponding feature-type has no 'elements'
  // so temove this here
  public IFeatureWrapperCollection<ET> getElements( );
}
