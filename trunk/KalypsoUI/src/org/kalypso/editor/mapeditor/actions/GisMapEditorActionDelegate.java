package org.kalypso.editor.mapeditor.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.IWorkbenchPart;
import org.kalypso.editor.mapeditor.GisMapEditor;

/**
 * @author doemming
 */
public abstract class GisMapEditorActionDelegate implements IEditorActionDelegate, ISelectionListener
{
  protected GisMapEditor myEditor=null;
  /**
   * @see org.eclipse.ui.IEditorActionDelegate#setActiveEditor(org.eclipse.jface.action.IAction, org.eclipse.ui.IEditorPart)
   */
  public void setActiveEditor( IAction action, IEditorPart targetEditor )
  {
    System.out.println("setActiveEditor... wenn der Editor wechselt...");
    myEditor=(GisMapEditor) targetEditor;
  }

  /**
   * @see org.eclipse.ui.ISelectionListener#selectionChanged(org.eclipse.ui.IWorkbenchPart, org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IWorkbenchPart part, ISelection selection )
  {
    System.out.println("selectionChanged(IWorkbenchpart,ISelection...");
     
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
    System.out.println("selectionChanged(IAction,ISelection...");
      
  }

}
