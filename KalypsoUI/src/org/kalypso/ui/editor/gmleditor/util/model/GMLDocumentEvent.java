package org.kalypso.ui.editor.gmleditor.util.model;

import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;

public class GMLDocumentEvent
{
  protected FeatureElement gmlDocument;

  protected CommandableWorkspace workspace;

  public GMLDocumentEvent( FeatureElement receiver, CommandableWorkspace m_workspace )
  {
    gmlDocument = receiver;
    workspace = m_workspace;
  }

  public FeatureElement getGmlDocument()
  {
    return gmlDocument;
  }

  public CommandableWorkspace getWorkspace()
  {
    return workspace;
  }
}