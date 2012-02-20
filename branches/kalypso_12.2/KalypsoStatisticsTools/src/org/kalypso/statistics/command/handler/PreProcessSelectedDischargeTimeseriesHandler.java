package org.kalypso.statistics.command.handler;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchPart;
import org.joda.time.Duration;
import org.kalypso.statistics.db.handler.DBHandlerTimeseriesProfile;
import org.kalypso.statistics.gui.PartManager;
import org.kalypso.statistics.gui.views.timeseries.TimeseriesListView;
import org.kalypso.statistics.project.SessionDataProvider;
import org.kalypso.statistics.tools.TimeserieProcessor;
import org.kalypso.statistics.types.EDischargeTimestepArea;
import org.kalypso.statistics.types.ETimeseriesType;
import org.kalypso.statistics.types.data.TimeserieProfile;
import org.kalypso.statistics.utils.AppUtils;

public class PreProcessSelectedDischargeTimeseriesHandler extends AbstractHandler implements IHandler {

	public static final String ID = PreProcessSelectedDischargeTimeseriesHandler.class.getCanonicalName();

	@Override
	public Object execute(final ExecutionEvent event) throws ExecutionException {
		final IWorkbenchPart part = PartManager.getInstance().getActivePart();
		if (part != null && part instanceof TimeseriesListView) {
			final TimeseriesListView view = (TimeseriesListView) part;
			List<TimeserieProfile> profiles = new ArrayList<TimeserieProfile>();
			for (TimeserieProfile profile : view.getSelection()) {
				if (ETimeseriesType.PRECIPITATION.equals(profile.getTimeseriesType())) {
					profiles.add(profile);
				}
			}
			if (profiles.size() == 0) {
				MessageDialog.openWarning(Display.getDefault().getActiveShell(), AppUtils.APPLICATION_TITLE,
						"No precipitation timeseries are selected for processing.");
				return Status.OK_STATUS;
			}

			final ProgressMonitorDialog dialog = new ProgressMonitorDialog(Display.getDefault().getActiveShell());
			dialog.setCancelable(true);
			dialog.open();
			final IProgressMonitor monitor = dialog.getProgressMonitor();
			monitor.beginTask("Preprocessing...", IProgressMonitor.UNKNOWN);
			Display.getDefault().readAndDispatch();
			for (TimeserieProfile timeserieProfile : profiles) {
				if (timeserieProfile.getEntries().size() == 0) {
					DBHandlerTimeseriesProfile.loadEntries(timeserieProfile);
				}
				TimeserieProcessor processor = new TimeserieProcessor(timeserieProfile, true);
				final List<TimeserieProfile> dischargeAreaSeries = new ArrayList<TimeserieProfile>();
				Duration duration = new Duration(timeserieProfile.getTimestepMillis());
				if (duration.getStandardMinutes() == 5) {
					dischargeAreaSeries.addAll(processor.transformToDischargeAreaSeries(timeserieProfile, EDischargeTimestepArea.AREA_MINUTES, monitor));
				} else if (duration.getStandardHours() == 1) {
					dischargeAreaSeries.addAll(processor.transformToDischargeAreaSeries(timeserieProfile, EDischargeTimestepArea.AREA_HOURS, monitor));
				} else if (duration.getStandardDays() == 1) {
					dischargeAreaSeries.addAll(processor.transformToDischargeAreaSeries(timeserieProfile, EDischargeTimestepArea.AREA_DAYS, monitor));
				} else {
					// TODO report the problem
				}
				DBHandlerTimeseriesProfile dbHandlerTimeseriesProfile = SessionDataProvider.getInstance().getDataProvider().getDbHandlerTimeseriesProfile();
				dbHandlerTimeseriesProfile.saveRecords(dischargeAreaSeries);
			}
			monitor.done();
			dialog.close();

			view.deselectAll();
			view.refresh();

			// MessageDialog.openInformation(Display.getDefault().getActiveShell(),
			// AppUtils.APPLICATION_TITLE,
			// "This function is not implemented yet.");
		}
		return Status.OK_STATUS;
	}
}
