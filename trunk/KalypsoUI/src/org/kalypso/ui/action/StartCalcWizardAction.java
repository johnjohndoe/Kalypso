package org.kalypso.ui.action;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
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
    final ISelection selection = m_window.getSelectionService().getSelection(
        IPageLayout.ID_RES_NAV );

    final IProject[] projects = ResourceUtilities.findeProjectsFromSelection( selection );

    if( projects == null || projects.length != 1 )
    {
      MessageDialog.openInformation( m_window.getShell(), "Hochwasser Vorhersage durchführen",
          "Bitte wählen Sie genau ein Projekt im Navigator aus" );
      return;
    }

    final CalcWizard wizard = new CalcWizard( projects[0] );

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