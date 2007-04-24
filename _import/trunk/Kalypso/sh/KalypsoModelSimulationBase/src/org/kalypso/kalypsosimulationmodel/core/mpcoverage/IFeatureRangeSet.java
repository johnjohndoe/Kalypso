
package org.kalypso.kalypsosimulationmodel.core.mpcoverage;

import java.util.List;

import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

/**
 * Interface for classes which represents wbGml:FeatureRangeSet 
 * 
 * @author Patrice Congo
 */
public interface IFeatureRangeSet<RangeSetCls extends IFeatureWrapper2>
					extends List<RangeSetCls>, IFeatureWrapper2
{
	
}
