package org.kalypso.ogc.gml.convert;

import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author belger
 */
public interface ISourceHandler
{
  public GMLWorkspace getWorkspace() throws SourceHandlerException;
}
