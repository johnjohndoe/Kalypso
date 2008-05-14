package org.kalypso.model.wspm.sobek.result.processing;

import nl.wldelft.fews.pi.TimeSeriesComplexType;

import org.eclipse.core.runtime.CoreException;

public interface ISobekResultModel
{
  /**
   * @param crossSectionNodeId
   *            id of cross section node
   * @return
   */
  TimeSeriesComplexType getCrossSectionTimeSeries( String crossSectionNodeId ) throws CoreException;
}
