package org.kalypso.kalypso1d2d.pjt.actions;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.wizards.IWizardDescriptor;

/**
 * Starts the import roughness wizard
 * 
 * @author Stefan Kurzbach
 */
public class ImportRoughnessHandler extends WorkflowCommandHandler {
    private static final String ROUGHNESS_IMPORT_WIZARD_ID = "org.kalypso.ui.wizards.imports.roughness.ImportWizard";

    /**
     * @see org.kalypso.kalypsomodel1d2d.ui.WorkflowCommandHandler#executeInternal(org.eclipse.core.commands.ExecutionEvent)
     */
    @Override
    protected IStatus executeInternal(final ExecutionEvent event)
            throws CoreException {
        final IEvaluationContext context = (IEvaluationContext) event
                .getApplicationContext();
        final IStructuredSelection selection = (IStructuredSelection) context
                .getVariable(ISources.ACTIVE_CURRENT_SELECTION_NAME);
        final IWorkbenchWindow workbenchWindow = (IWorkbenchWindow) context
                        .getVariable(ISources.ACTIVE_WORKBENCH_WINDOW_NAME);
        final IWorkbench workbench = (workbenchWindow)
                .getWorkbench();
        final IWizardDescriptor wizardDescriptor = workbench
                .getNewWizardRegistry().findWizard(ROUGHNESS_IMPORT_WIZARD_ID);
        final INewWizard wizard = (INewWizard) wizardDescriptor.createWizard();
        final WizardDialog wizardDialog = new WizardDialog(workbenchWindow.getShell(), wizard);
        wizard.init(workbench, selection);
        if (wizardDialog.open() != Window.OK) {
            return Status.CANCEL_STATUS;
        } else {
            return Status.OK_STATUS;
        }

    }
}
