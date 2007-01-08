package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.util.List;

import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapper;

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
	//to see
	public  List<CT> getContainers();
	public  List<ET> getElements();
}
