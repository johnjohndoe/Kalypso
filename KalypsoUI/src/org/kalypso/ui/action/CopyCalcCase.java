package org.kalypso.ui.action;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.actions.CopyFilesAndFoldersOperation;
import org.kalypso.ui.nature.ModelNature;

/**
 * @author bce
 */
public class CopyCalcCase implements IWorkbenchWindowActionDelegate
{
  private IWorkbenchWindow m_window;

  /**
   * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#dispose()
   */
  public void dispose()
  {
    // nichts tun
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
    final ISelection selection = m_window.getSelectionService().getSelection(
        IPageLayout.ID_RES_NAV );

    IResource resource = null;
    if( selection instanceof IStructuredSelection )
    {
      final IStructuredSelection struct = (IStructuredSelection)selection;
      if( struct.size() == 1 )
        resource = (IResource)struct.getFirstElement();
    }
    
    if( resource == null || !( resource instanceof IFolder ) )
    {
      MessageDialog.openInformation( m_window.getShell(), "Rechenvariante anlegen",
      "Bitte wählen Sie einen Rechenfall im Navigator aus" );
      return;
    }
    
    final IFolder folder = (IFolder)resource;
    final IFile file = folder.getFile( ModelNature.CONTROL_NAME );
    if( !file.exists() )
    {
      MessageDialog.openInformation( m_window.getShell(), "Rechenvariante anlegen",
      "Bitte wählen Sie einen Rechenfall im Navigator aus" );
      return;
    }
    
    final CopyFilesAndFoldersOperation operation = new CopyFilesAndFoldersOperation( m_window.getShell() );
    operation.copyResources( new IResource[] { resource }, resource.getParent() );    
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
    // ignore
  }
}
