/*
 * Created on 15.07.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.panels;

import java.util.EventListener;

/**
 * @author F.Lindemann
 *  
 */
public interface PanelListener extends EventListener
{
  public void valueChanged( PanelEvent event );
}