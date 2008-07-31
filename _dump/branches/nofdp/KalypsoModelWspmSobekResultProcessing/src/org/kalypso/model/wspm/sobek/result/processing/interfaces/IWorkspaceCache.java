package org.kalypso.model.wspm.sobek.result.processing.interfaces;

import javax.xml.bind.JAXBElement;

import nl.wldelft.fews.pi.TimeSeriesComplexType;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.GMLWorkspace;

public interface IWorkspaceCache
{
  public CommandableWorkspace getCommandableWorkspace( String id );

  public void registerWorkspaces( String id, GMLWorkspace gml, CommandableWorkspace commandable );

  public JAXBElement<TimeSeriesComplexType> getPiStructuresElement( ) throws CoreException;

  public JAXBElement<TimeSeriesComplexType> getPiCalculationPointElement( ) throws CoreException;
}
