
package org.kalypso.kalypsosimulationmodel.core.mpcoverage;

import java.util.List;

import org.kalypsodeegree.model.feature.binding.IFeatureWrapper;

/**
 * Interface for classes which represents wbGml:FeatureRangeSet 
 * 
 * @author Patrice Congo
 */
public interface IFeatureRangeSet<RangeSetCls extends IFeatureWrapper>
					extends List<RangeSetCls>, IFeatureWrapper
{
	
}
