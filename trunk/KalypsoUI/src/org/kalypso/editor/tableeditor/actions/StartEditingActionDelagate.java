package org.kalypso.editor.tableeditor.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;

/**
 * @author belger
 */
public class StartEditingActionDelagate extends GisTableAbstractActionDelagate
{
  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    getEditor().getTheme().setEditing( action.isChecked() );
  }
  
  protected boolean isChecked()
  {
    return getEditor().getTheme() != null && getEditor().getTheme().isEditing();
  }

  /**
   * @see org.kalypso.editor.tableeditor.actions.GisTableAbstractActionDelagate#isEnabled(org.eclipse.jface.viewers.ISelection)
   */
  protected boolean isEnabled( final ISelection selection )
  {
    return getEditor().getTheme() != null;
  }
}