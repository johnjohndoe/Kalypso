package org.kalypso.kalypsosimulationmodel.core.discr;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;

public interface IFENetItem extends Feature
{
  public GM_Object recalculateElementGeometry( ) throws GM_Exception;
}
