package org.kalypso.ui.editor;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;

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
   * @return shell
   */
  public Shell getShell()
  {
    return m_editor.getSite().getShell();
  }
  
  /**
   * @return editor
   */
  public IEditorPart getEditor()
  {
    return m_editor;
  }
}
