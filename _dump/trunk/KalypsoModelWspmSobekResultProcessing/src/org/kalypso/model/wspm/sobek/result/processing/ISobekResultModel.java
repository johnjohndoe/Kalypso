package org.kalypso.model.wspm.sobek.result.processing;

import nl.wldelft.fews.pi.TimeSeriesComplexType;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.model.wspm.sobek.core.interfaces.ICrossSectionNode;
import org.kalypso.model.wspm.sobek.result.processing.model.IResultTimeSeries;

public interface ISobekResultModel
{
  /**
   * @param crossSectionNodeId
   *            id of cross section node
   * @return
   */
  TimeSeriesComplexType getCrossSectionBinding( ICrossSectionNode node ) throws CoreException;

  IResultTimeSeries getCrossSectionTimeSeries( ICrossSectionNode node ) throws CoreException;

}
