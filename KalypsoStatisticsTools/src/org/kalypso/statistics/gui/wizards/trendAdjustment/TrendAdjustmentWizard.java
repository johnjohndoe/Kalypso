package org.kalypso.statistics.gui.wizards.trendAdjustment;

import java.util.List;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPart;
import org.kalypso.statistics.db.handler.DBHandlerTimeseriesProfile;
import org.kalypso.statistics.gui.PartManager;
import org.kalypso.statistics.gui.views.timeseries.TimeseriesListView;
import org.kalypso.statistics.project.SessionDataProvider;
import org.kalypso.statistics.tools.Trend;
import org.kalypso.statistics.types.ETrendAdjustmentType;
import org.kalypso.statistics.types.data.TimeserieProfile;
import org.kalypso.statistics.types.data.TimeserieProfileEntry;
import org.kalypso.statistics.utils.AppUtils;
import org.kalypso.zml.ui.imports.ImportObservationWizard;

public class TrendAdjustmentWizard extends ImportObservationWizard {

	public static final String ID = TrendAdjustmentWizard.class.getCanonicalName();
	private TrendAdjustmentWizardPage m_page = null;

	@Override
	public void init(IWorkbench workbench, IStructuredSelection currentSelection) {
		super.init(workbench, currentSelection);
		setWindowTitle(AppUtils.APPLICATION_TITLE);
	}

	@Override
	public void addPages() {
		m_page = new TrendAdjustmentWizardPage("");
		addPage(m_page);
	}

	@Override
	public boolean performFinish() {
		try {
			ETrendAdjustmentType adjustmentType = m_page.getSelectedType();
			if (adjustmentType == null)
				return false;
			final IWorkbenchPart activePart = PartManager.getInstance().getActivePart();
			if (!(activePart instanceof TimeseriesListView)) {
				// TODO pop error message
				return true;
			}
			boolean makeACopy = m_page.makeACopy();
			for (TimeserieProfile timeserie : ((TimeseriesListView) activePart).getSelection()) {
				DBHandlerTimeseriesProfile.loadEntries(timeserie);
				final List<TimeserieProfileEntry> entries = timeserie.getEntries();
				if (entries.size() == 0) {
					continue;
				}
				TimeserieProfile adjustedProfile = Trend.doTrendAdjustment(timeserie, adjustmentType);
				adjustedProfile.setNodeProfileUID(timeserie.getNodeProfileUID());
				if (adjustedProfile.getEntries().size() > 0) {
					adjustedProfile.setNumberOfEntries(adjustedProfile.getEntries().size());
					adjustedProfile.setTimeFrom(adjustedProfile.getEntries().get(0).getTime());
					adjustedProfile.setTimeTo(adjustedProfile.getEntries().get(adjustedProfile.getEntries().size() - 1).getTime());
				}
				adjustedProfile.setUID(makeACopy ? 0 : timeserie.getUID());
				SessionDataProvider.getInstance().getDataProvider().getDbHandlerTimeseriesProfile().saveRecord(adjustedProfile);
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
