package org.kalypso.editor.tableeditor.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.kalypso.editor.tableeditor.command.SaveDataCommand;

/**
 * @author belger
 */
public class SaveDataActionDelagate extends GisTableAbstractActionDelagate
{
  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    if( !MessageDialog.openConfirm( getEditor().getSite().getShell(), "Daten speichern", "Sollen die Daten wirklich gespeichert werden?" ) )
      return;
    
    getEditor().postCommand( new SaveDataCommand( getEditor() ), null );
  }

  /**
   * @see org.kalypso.editor.tableeditor.actions.GisTableAbstractActionDelagate#isEnabled(org.eclipse.jface.viewers.ISelection)
   */
  protected boolean isEnabled( final ISelection selection )
  {
    return true;
  }
}