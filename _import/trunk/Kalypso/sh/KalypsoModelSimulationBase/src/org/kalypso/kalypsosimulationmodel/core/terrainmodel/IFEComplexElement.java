package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.util.List;

/**
 * Base tagging interface  for all classes abstracting a finite
 * element element in the substitution hierarchy of simBase:_FENode 
 * 
 * @author Patrice Congo
 */
public interface IFEComplexElement<	CT extends IFEComplexElement, 
									ET extends IFEElement>
{
	//to see
	public  List<CT> getContainers();
	public  List<ET> getElements();
}
