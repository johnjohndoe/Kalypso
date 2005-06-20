package org.kalypso.ui.editor.gmleditor.util.model;

import java.util.EventListener;

public interface IGMLDocumentListener extends EventListener
{
  public void onChange( GMLDocumentEvent event );
}