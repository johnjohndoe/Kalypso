package org.kalypso.ui.action;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.kalypso.ui.calcwizard.CalcWizard;
import org.kalypso.ui.calcwizard.CalcWizardDialog;

/**
 * @author belger
 */
public class StartCalcWizardAction implements IWorkbenchWindowActionDelegate
{
  private IWorkbenchWindow m_window;

  /**
   * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#dispose()
   */
  public void dispose()
  {
  // nix zu tun
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
    IProject project = null;

    final ISelection selection = m_window.getSelectionService().getSelection(
        IPageLayout.ID_RES_NAV );

    if( !selection.isEmpty() && selection instanceof IStructuredSelection )
    {
      final IStructuredSelection ssel = (IStructuredSelection)selection;
      if( ssel.size() == 1 )
      {
        final Object firstElement = ssel.getFirstElement();
        if( firstElement instanceof IProject )
          project = (IProject)firstElement;
        else if( firstElement instanceof IFile || firstElement instanceof IFolder )
          project = ( (IContainer)firstElement ).getProject();
      }
    }

    if( project == null )
    {
      MessageDialog.openInformation( m_window.getShell(), "Hochwasser Vorhersage durchführen",
          "Bitte wählen Sie genau ein Projekt im Navigator aus" );
      return;
    }

    final CalcWizard wizard = new CalcWizard( project );

    final WizardDialog dialog = new CalcWizardDialog( m_window.getShell(), wizard ); 
    dialog.open();
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( final IAction action, final ISelection selection )
  {
  // mir wurscht
  }

}