package org.kalypso.ui.editor.obstableeditor.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;
import org.kalypso.ui.editor.obstableeditor.ObservationTableEditor;

/**
 * AbstractEditorActionDelegate
 * 
 * @author schlienger
 */
public abstract class AbstractEditorActionDelegate implements
    IEditorActionDelegate
{
  private IEditorPart m_editor;

  /**
   * @see org.eclipse.ui.IEditorActionDelegate#setActiveEditor(org.eclipse.jface.action.IAction, org.eclipse.ui.IEditorPart)
   */
  public void setActiveEditor( IAction action, IEditorPart targetEditor )
  {
    m_editor = targetEditor;
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
    // 
  }
  
  /**
   * @return editor
   */
  public ObservationTableEditor getEditor()
  {
    return (ObservationTableEditor) m_editor;
  }
}
