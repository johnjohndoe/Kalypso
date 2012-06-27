package org.kalypso.statistics.gui.wizards.createExtremsTimeseries;

import java.util.List;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPart;
import org.joda.time.Duration;
import org.kalypso.statistics.db.handler.DBHandlerTimeseriesProfile;
import org.kalypso.statistics.gui.PartManager;
import org.kalypso.statistics.gui.views.timeseries.TimeseriesListView;
import org.kalypso.statistics.project.SessionDataProvider;
import org.kalypso.statistics.tools.TimeserieProcessor;
import org.kalypso.statistics.types.EHydrologicalInterval;
import org.kalypso.statistics.types.ETimeseriesType;
import org.kalypso.statistics.types.data.TimeserieProfile;
import org.kalypso.statistics.types.data.TimeserieProfileEntry;
import org.kalypso.statistics.utils.AppUtils;
import org.kalypso.zml.ui.imports.ImportObservationWizard;

public class CreateExtemsTimeseriesWizard extends ImportObservationWizard {

	public static final String ID = CreateExtemsTimeseriesWizard.class.getCanonicalName();
	private CreateExtemsTimeseriesWizardPage m_page = null;

	@Override
	public void init(IWorkbench workbench, IStructuredSelection currentSelection) {
		super.init(workbench, currentSelection);
		setWindowTitle(AppUtils.APPLICATION_TITLE);
	}

	@Override
	public void addPages() {
		m_page = new CreateExtemsTimeseriesWizardPage("");
		addPage(m_page);
	}

	@Override
	public boolean performFinish() {
		try {
			EHydrologicalInterval intervalType = m_page.getSelectedIntervalType();
			if (intervalType == null)
				return false;
			final IWorkbenchPart activePart = PartManager.getInstance().getActivePart();
			if (!(activePart instanceof TimeseriesListView)) {
				// TODO pop error message
				return true;
			}
			boolean coefficientOverTheWholePeriod = m_page.coefficientOverTheWholePeriod();
			if (coefficientOverTheWholePeriod) {
				// /////////////////////////////////////////////////////////////////////////////////
				// TODO what to do here?
				// /////////////////////////////////////////////////////////////////////////////////
			}
			for (TimeserieProfile timeserie : ((TimeseriesListView) activePart).getSelection()) {
				DBHandlerTimeseriesProfile.loadEntries(timeserie);
				final List<TimeserieProfileEntry> entries = timeserie.getEntries();
				if (entries.size() == 0) {
					continue;
				}
				Duration duration = new Duration(entries.get(0).getTime().getTimeInMillis(), entries.get(entries.size() - 1).getTime().getTimeInMillis());
				try {
					int years = (int) (duration.getStandardDays() / 365);
					int coefficient = 1;
					final TimeserieProcessor processor = new TimeserieProcessor(timeserie, true);
					switch (intervalType) {
					case CALENDAR_YEAR:
					case HYDROLOGIC_YEAR_NH:
						coefficient = 1;
						break;
					case HYDROLOGIC_SUMMER_NH:
					case HYDROLOGIC_WINTER_NH:
						switch (timeserie.getTimeseriesType()) {
						case PRECIPITATION:
							coefficient = 3;
							processor.getTimeserieProfile().setTimeseriesType(ETimeseriesType.PRECIPITATION_EXTREMS);
							break;
						case WATERLEVEL:
							coefficient = 5;
							processor.getTimeserieProfile().setTimeseriesType(ETimeseriesType.WATERLEVEL_EXTREMS);
							break;
						default:
							coefficient = 1;
							break;
						}
						break;
					}
//					final int numberOfExtremsPerPeriod = years * coefficient;
					final int numberOfExtremsPerPeriod = coefficient;
					processor.toIntervalMaximums(numberOfExtremsPerPeriod, intervalType);
					
					int size = processor.getTimeserieProfile().getEntries().size();
					processor.getTimeserieProfile().setNumberOfEntries(size);
					processor.getTimeserieProfile().setDescription(String.format("%s based exteme values from '%s'", intervalType.getShortLabel(), timeserie.getName()));
					
					SessionDataProvider.getInstance().getDataProvider().getDbHandlerTimeseriesProfile().saveRecord(processor.getTimeserieProfile());
					timeserie.getEntries().clear();
				} catch (ClassCastException e) {
					// too long period
					AppUtils.getLogger().logException(e);
					// TODO pop the message box
				}
			}

			// refresh view
			IWorkbenchPart part = PartManager.getInstance().getActivePart();
			if (part instanceof TimeseriesListView) {
				TimeseriesListView view = (TimeseriesListView) part;
				view.refresh();
			}

		} catch (final Exception e) {
			Display.getDefault().asyncExec(new Runnable() {
				@Override
				public void run() {
					MessageDialog.openError(Display.getDefault().getActiveShell(), AppUtils.APPLICATION_TITLE, "Error occured: " + e.getMessage());
				}
			});
			AppUtils.getLogger().logException(e);
			return false;
		}
		return true;
	}
}
