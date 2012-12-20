package org.kalypso.ui.wizards.imports.roughness;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

/**
 * Class associated with the popupMenu for the folder
 * Start the wizard in the run method
 */

public class ImportAction implements IObjectActionDelegate 
{
	IWorkbenchPart part;
	ISelection selection;

	/**
	 * @see IObjectActionDelegate#setActivePart(IAction, IWorkbenchPart)
	 */
	@Override
  public void setActivePart(IAction action, IWorkbenchPart wbPart) {
			this.part = wbPart;
	}

	/**
	 * @see IActionDelegate#run(IAction)
	 * Instantiates the wizard and opens it in the wizard container
	 */
	@Override
  public void run(IAction action) {
        ImportWizard wizard = new ImportWizard();
		if ((selection instanceof IStructuredSelection) || (selection == null))
		wizard.init(part.getSite().getWorkbenchWindow().getWorkbench(), 
			(IStructuredSelection)selection);
			
		// Instantiates the wizard container with the wizard and opens it
		WizardDialog dialog = new WizardDialog( part.getSite().getShell(), wizard);
		dialog.create();
		dialog.open();
	}

	/**
	 * @see IActionDelegate#selectionChanged(IAction, ISelection)
	 */
	@Override
  public void selectionChanged(IAction action, ISelection iSelection) {
		this.selection = iSelection;
	}

}
