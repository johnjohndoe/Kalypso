package org.kalypso.ui.action;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.actions.CopyProjectOperation;

/**
 * @author bce
 */
public class CopyModelDelegate implements IWorkbenchWindowActionDelegate
{

  private IWorkbenchWindow m_window;

  /**
   * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#dispose()
   */
  public void dispose()
  {
  // nichts zu tun  
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#init(org.eclipse.ui.IWorkbenchWindow)
   */
  public void init( IWorkbenchWindow window )
  {
    m_window = window;
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    try
    {
      final IProject project = ModelActionHelper.chooseOneProject( m_window );

      final CopyProjectOperation op = new CopyProjectOperation( m_window.getShell() );
      op.copyProject( project );
    }
    catch( final CoreException ce )
    {
      ErrorDialog.openError( m_window.getShell(), "Modell kopieren", "Modell konnte nicht kopiert werden", ce.getStatus() );
    }
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
    // ignore
  }

}
