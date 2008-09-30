package org.kalypso.model.wspm.sobek.result.processing.interfaces;

import nl.wldelft.fews.pi.TimeSerieComplexType;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.model.wspm.sobek.core.interfaces.IEmptyNode;
import org.kalypso.model.wspm.sobek.core.interfaces.INode;
import org.kalypso.model.wspm.sobek.result.processing.model.IBranchLengthSectionModel;
import org.kalypso.model.wspm.sobek.result.processing.model.IResultTimeSeries;

public interface ISobekResultModel
{
  /**
   * @param crossSectionNodeId
   *          id of cross section node
   * @return
   */
  IResultTimeSeries getNodeTimeSeries( INode node ) throws CoreException;

  TimeSerieComplexType getNodeBinding( INode node ) throws CoreException;

  IBranchLengthSectionModel getBranchHydrographModel( ) throws CoreException;

  public void dispose( );

  IPolderNodeResultWrapper getPolderNodeResult( IEmptyNode node );

  IRetardingBasinNodeResultWrapper getRetardingBasinNodeResult( IEmptyNode node );

  IWeirNodeResultWrapper getWeirNodeResult( IEmptyNode node );
}
