package org.kalypso.statistics.command.handler;

import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.WorkbenchException;
import org.kalypso.statistics.gui.PartManager;
import org.kalypso.statistics.gui.views.timeseries.TimeseriesListView;
import org.kalypso.statistics.types.data.TimeserieProfile;
import org.kalypso.statistics.utils.AppUtils;
import org.kalypso.statistics.utils.WorkbenchUtils;

public class ChartDisplaySelectedTimeseriesHandler extends AbstractHandler implements IHandler {

	public static final String ID = ChartDisplaySelectedTimeseriesHandler.class.getCanonicalName();

	@Override
	public Object execute(final ExecutionEvent event) throws ExecutionException {
		final IWorkbenchPart part = PartManager.getInstance().getActivePart();
		if (part != null && part instanceof TimeseriesListView) {
			final TimeseriesListView view = (TimeseriesListView) part;
			List<TimeserieProfile> profiles = view.getSelection();
			if (profiles.size() == 0) {
				MessageDialog.openWarning(Display.getDefault().getActiveShell(), AppUtils.APPLICATION_TITLE, "No timeseries are selected for displaying.");
				return Status.OK_STATUS;
			}
			try {
				WorkbenchUtils.showTimeseries("Multiple timeseries", profiles);
			} catch (WorkbenchException e) {
				MessageDialog.openWarning(Display.getDefault().getActiveShell(), AppUtils.APPLICATION_TITLE, "Cannot open Chart View.");
				return Status.OK_STATUS;
			}
		}
		return Status.OK_STATUS;
	}

}
