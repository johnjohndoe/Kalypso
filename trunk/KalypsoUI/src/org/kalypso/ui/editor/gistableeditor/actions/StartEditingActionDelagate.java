package org.kalypso.ui.editor.gistableeditor.actions;

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
//    getEditor().getLayerTable().getTheme().setEditing( action.isChecked() );
  }
  
  protected boolean isChecked()
  {
    return false;
//    return getEditor().getLayerTable().getTheme() != null && getEditor().getLayerTable().getTheme().isEditing();
  }

  /**
   * @see org.kalypso.ui.editor.gistableeditor.actions.GisTableAbstractActionDelagate#isEnabled(org.eclipse.jface.viewers.ISelection)
   */
  protected boolean isEnabled( final ISelection selection )
  {
    return getEditor().getLayerTable().getTheme() != null;
  }
}