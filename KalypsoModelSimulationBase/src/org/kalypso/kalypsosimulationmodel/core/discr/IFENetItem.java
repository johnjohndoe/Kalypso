package org.kalypso.kalypsosimulationmodel.core.discr;

import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;

public interface IFENetItem extends IFeatureWrapper2
{
  public GM_Object recalculateElementGeometry( ) throws GM_Exception;
}
