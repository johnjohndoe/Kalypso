package org.kalypso.ui.action;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Button;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.kalypso.ui.wizard.calccase.NewCalculationCaseWizard;

/**
 * @author belger
 */
public class NewCalcCaseAction implements IWorkbenchWindowActionDelegate
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
  public void init( final IWorkbenchWindow window )
  {
    m_window = window;
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    // selektiertes verzeichnis finden
    final ISelection selection = m_window.getSelectionService().getSelection( IPageLayout.ID_RES_NAV );

    // NewCalcCaseWizard starten
    final NewCalculationCaseWizard wizard = new NewCalculationCaseWizard(  );
    wizard.init( m_window.getWorkbench(), (IStructuredSelection)selection );

    final WizardDialog dlg = new WizardDialog( m_window.getShell(), wizard )
    {
      /**
       * @see org.eclipse.jface.wizard.WizardDialog#updateButtons()
       */
      public void updateButtons()
      {
        super.updateButtons();
        
        final Button nextButton = getButton( IDialogConstants.NEXT_ID );
        getShell().setDefaultButton( nextButton );
      }
    };

    dlg.open();
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    // mir egal
  }
}
