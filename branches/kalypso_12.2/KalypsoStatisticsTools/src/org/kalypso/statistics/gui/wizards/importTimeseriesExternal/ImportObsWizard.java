package org.kalypso.statistics.gui.wizards.importTimeseriesExternal;

import java.io.File;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPart;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.adapter.INativeObservationAdapter;
import org.kalypso.statistics.gui.PartManager;
import org.kalypso.statistics.gui.views.timeseries.TimeseriesListView;
import org.kalypso.statistics.project.SessionDataProvider;
import org.kalypso.statistics.types.ETimeseriesType;
import org.kalypso.statistics.types.data.NodeProfile;
import org.kalypso.statistics.types.data.TimeserieProfile;
import org.kalypso.statistics.types.data.TimeserieProfileEntry;
import org.kalypso.statistics.utils.AppUtils;
import org.kalypso.zml.ui.imports.ImportObservationWizard;
import org.kalypso.zml.ui.imports.ObservationImportSelection;

public class ImportObsWizard extends ImportObservationWizard {

	public static final String ID = ImportObsWizard.class.getCanonicalName();
	private ImportObsWizardPage m_page = null;

	@Override
	public void init(IWorkbench workbench, IStructuredSelection currentSelection) {
		super.init(workbench, currentSelection);
		setWindowTitle(AppUtils.APPLICATION_TITLE);
	}

	@Override
	public void addPages() {
		m_page = new ImportObsWizardPage("Page name");
		addPage(m_page);
	}

	@Override
	public boolean performFinish() {
		try {
			final ObservationImportSelection selection = (ObservationImportSelection) m_page.getSelection();
			final File fileSource = selection.getFileSource();
			final INativeObservationAdapter nativaAdapter = selection.getNativeAdapter();
			final TimeZone timezone = selection.getSourceTimezone();
			final IObservation srcObservation = nativaAdapter.createObservationFromSource(fileSource, timezone, false);

			String obsName = srcObservation.getName();
			if (obsName == null)
				obsName = fileSource.getName();
			// final MetadataList obsMetadataList =
			// srcObservation.getMetadataList();
			// for (final Entry<Object, Object> entry :
			// obsMetadataList.entrySet()) {
			// final String mdKey = (String) entry.getKey();
			// final String mdValue = (String) entry.getValue();
			// System.out.println(String.format("%s --> %s", mdKey, mdValue));
			// }

			final IAxis[] axes = srcObservation.getAxes();
			String valueType = null;
			for (IAxis axis : axes) {
				if (Double.class.isAssignableFrom(axis.getDataClass())) {
					valueType = axis.getType();
					break;
				}
			}
			if (valueType == null) {
				throw new IllegalArgumentException("Timeseries structure currently not supported [1]");
			}
			ETimeseriesType timeseriesType = ETimeseriesType.UNKNOWN;
			for (ETimeseriesType type : ETimeseriesType.values()) {
				if (type.getAbbreviation().equals(valueType)) {
					timeseriesType = type;
					break;
				}
			}
			boolean isFirst = false;
			boolean isLast = false;
			Calendar first = null;
			Calendar last = null;
			final TimeserieProfile profile = new TimeserieProfile(0, obsName, timeseriesType);
			final ITupleModel model = srcObservation.getValues(null);
			int modelSize = model.size();
			for (int i = 0; i < modelSize; i++) {
				isFirst = i == 0;
				isLast = i == (modelSize - 1);
				Calendar time = null;
				Double value = null;
				for (final IAxis axis : srcObservation.getAxes()) {
					if (axis.isPersistable()) {
						if (Date.class.isAssignableFrom(axis.getDataClass())) {
							time = Calendar.getInstance(timezone);
							time.setTimeInMillis(((Date) model.get(i, axis)).getTime());
							if (isFirst)
								first = time;
							if (isLast)
								last = time;
						} else if (Double.class.isAssignableFrom(axis.getDataClass())) {
							value = (Double) model.get(i, axis);
						} else {
							throw new IllegalArgumentException("Data type currently not supported");
						}
					}
				}
				if (time != null && value != null) {
					profile.getEntries().add(new TimeserieProfileEntry(0, time, value));
				} else {
					throw new IllegalArgumentException("Timeseries structure currently not supported [2]");
				}
			}
			profile.setTimeFrom(first);
			profile.setTimeTo(last);
			profile.setNumberOfEntries(modelSize);
			if (profile.getEntries().size() > 1) {
				// here we assume that the time-steps are regular!
				profile.setTimestepMillis(profile.getEntries().get(1).getTime().getTimeInMillis() - profile.getEntries().get(0).getTime().getTimeInMillis());
			}
			NodeProfile node = m_page.getParentNode();
			if (node != null) {
				profile.setNodeProfileUID(node.getUID());
			}
			SessionDataProvider.getInstance().getDataProvider().getDbHandlerTimeseriesProfile().saveRecord(profile);

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
			e.printStackTrace();
			return false;
		}
		return true;
	}
}
