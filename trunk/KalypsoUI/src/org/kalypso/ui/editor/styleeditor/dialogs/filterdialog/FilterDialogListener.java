package org.kalypso.ui.editor.styleeditor.dialogs.filterdialog;

import java.util.EventListener;

/**
 * @author F.Lindemann
 *  
 */
public interface FilterDialogListener extends EventListener
{

  public void filterUpdated( FilterDialogEvent event );
}