package org.kalypso.model.wspm.sobek.result.processing.interfaces;

import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.GMLWorkspace;

public interface IWorkspaceCache
{

  CommandableWorkspace getCommandableWorkspace( String id );

  void registerWorkspaces( String id, GMLWorkspace gml, CommandableWorkspace commandable );

}
