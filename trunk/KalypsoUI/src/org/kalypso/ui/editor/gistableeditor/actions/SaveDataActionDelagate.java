package org.kalypso.ui.editor.gistableeditor.actions;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.progress.IProgressService;
import org.kalypso.ui.editor.gistableeditor.GisTableEditor;

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
    final GisTableEditor editor = getEditor();
    if( editor == null )
      return;

    final Shell shell = editor.getSite().getShell();
    if( !MessageDialog.openConfirm( shell, "Daten speichern",
        "Sollen die Daten wirklich gespeichert werden?" ) )
      return;

    final WorkspaceModifyOperation op = new WorkspaceModifyOperation()
    {
      protected void execute( final IProgressMonitor monitor ) throws CoreException
      {
        editor.getLayerTable().saveData( monitor );
      }
    };
    
    final IProgressService progressService = editor.getSite().getWorkbenchWindow().getWorkbench().getProgressService();
    try
    {
      progressService.busyCursorWhile( op );
    }
    catch( final InvocationTargetException e )
    {
      e.printStackTrace();
      
      final CoreException ce = (CoreException)e.getTargetException();
      ErrorDialog.openError( shell, "Fehler", "Fehler beim Speichern", ce.getStatus() );
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
    }

  }

  /**
   * @see org.kalypso.ui.editor.gistableeditor.actions.GisTableAbstractActionDelagate#isEnabled(org.eclipse.jface.viewers.ISelection)
   */
  protected boolean isEnabled( final ISelection selection )
  {
    return true;
  }

  /**
   * @see org.kalypso.ui.editor.gistableeditor.actions.GisTableAbstractActionDelagate#isChecked()
   */
  protected boolean isChecked()
  {
    return false;
  }
}