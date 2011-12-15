package org.kalypso.statistics.command.handler;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWizard;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.wizards.IWizardDescriptor;
import org.kalypso.statistics.gui.PartManager;
import org.kalypso.statistics.gui.views.timeseries.TimeseriesListView;
import org.kalypso.statistics.gui.wizards.createExtremsTimeseries.CreateExtemsTimeseriesWizard;
import org.kalypso.statistics.utils.AppUtils;

public class CreateExtremsTimeseriesHandler extends AbstractHandler implements IHandler {

	public static final String ID = CreateExtremsTimeseriesHandler.class.getCanonicalName();

	@Override
	public Object execute(final ExecutionEvent event) throws ExecutionException {
		Shell activeShell = Display.getDefault().getActiveShell();
		if (activeShell == null) {
			activeShell = new Shell(Display.getDefault());
		}
		final IWorkbenchPart part = PartManager.getInstance().getActivePart();
		if (part != null && part instanceof TimeseriesListView) {
			final TimeseriesListView view = (TimeseriesListView) part;
			if (view.getSelection().size() == 0) {
				MessageDialog.openWarning(activeShell, AppUtils.APPLICATION_TITLE, "No timeseries are selected for processing.");
				return Status.OK_STATUS;
			}
			final IWizardDescriptor wizardDesc = PlatformUI.getWorkbench().getNewWizardRegistry().findWizard(CreateExtemsTimeseriesWizard.ID);
			try {
				final IWorkbenchWizard wizard = wizardDesc.createWizard();
				final WizardDialog dialog = new WizardDialog(activeShell, wizard);
				dialog.open();
			} catch (final CoreException e1) {
				final IStatus status = e1.getStatus();
				ErrorDialog.openError(activeShell, AppUtils.APPLICATION_TITLE, e1.getMessage(), status);
			}
			view.refresh();
			view.deselectAll();
		}
		return Status.OK_STATUS;
	}

}
