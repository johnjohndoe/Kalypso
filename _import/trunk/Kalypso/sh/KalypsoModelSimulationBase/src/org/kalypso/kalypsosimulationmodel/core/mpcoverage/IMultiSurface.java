package org.kalypso.kalypsosimulationmodel.core.mpcoverage;

import java.util.List;

import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapper;
import org.kalypsodeegree.model.geometry.GM_Surface;

/**
 * Inteface for Java class represensting the multi surface feature 
 * wbGml:MulitSurface
 * 
 * @author Patrice Congo
 */
public interface IMultiSurface extends List<GM_Surface>, IFeatureWrapper
{
	
}
