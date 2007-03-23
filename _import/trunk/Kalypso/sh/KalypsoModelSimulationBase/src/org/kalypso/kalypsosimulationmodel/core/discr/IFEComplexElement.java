package org.kalypso.kalypsosimulationmodel.core.discr;

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
  //yes empty
}
