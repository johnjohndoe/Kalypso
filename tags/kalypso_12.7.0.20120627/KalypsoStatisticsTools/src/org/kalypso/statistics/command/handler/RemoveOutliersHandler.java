package org.kalypso.statistics.command.handler;

import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPart;
import org.kalypso.statistics.db.handler.DBHandlerTimeseriesProfile;
import org.kalypso.statistics.gui.PartManager;
import org.kalypso.statistics.gui.views.timeseries.TimeseriesListView;
import org.kalypso.statistics.project.SessionDataProvider;
import org.kalypso.statistics.tools.GrubbsTest;
import org.kalypso.statistics.tools.TimeserieProcessor;
import org.kalypso.statistics.types.ETimeseriesType;
import org.kalypso.statistics.types.data.TimeserieProfile;
import org.kalypso.statistics.utils.AppUtils;

public class RemoveOutliersHandler extends AbstractHandler implements IHandler {

	public static final String ID = RemoveOutliersHandler.class.getCanonicalName();

	@Override
	public Object execute(final ExecutionEvent event) throws ExecutionException {
		Shell activeShell = Display.getDefault().getActiveShell();
		if (activeShell == null) {
			activeShell = new Shell(Display.getDefault());
		}
		final IWorkbenchPart part = PartManager.getInstance().getActivePart();
		if (part != null && part instanceof TimeseriesListView) {
			final TimeseriesListView view = (TimeseriesListView) part;
			final List<TimeserieProfile> list = view.getSelection();
			if (list.size() == 0) {
				MessageDialog.openWarning(activeShell, AppUtils.APPLICATION_TITLE, "No timeseries are selected for processing.");
				return Status.OK_STATUS;
			}

			DBHandlerTimeseriesProfile dbHandler = SessionDataProvider.getInstance().getDataProvider().getDbHandlerTimeseriesProfile();
			for (int i = 0; i < list.size(); i++) {
				TimeserieProfile profile = list.get(i);
				final ETimeseriesType type = profile.getTimeseriesType();
				if (profile.getNumberOfEntries() < 600
						&& (type.equals(ETimeseriesType.PRECIPITATION_EXTREMS) || type.equals(ETimeseriesType.WATERLEVEL_EXTREMS))) {
					DBHandlerTimeseriesProfile.loadEntries(profile);
					TimeserieProcessor processor = new TimeserieProcessor(profile, true);
					profile.getEntries().clear();
					for (;;) {
						double grubbsMax001 = GrubbsTest.getGrubbsMax(processor.getTimeserieProfile(), 0.05);
						if (new Double(grubbsMax001).isNaN()) {
							break;
						} else {
							processor.removeValue(grubbsMax001);
						}
					}
					int size = processor.getTimeserieProfile().getEntries().size();
					processor.getTimeserieProfile().setNumberOfEntries(size);
					processor.getTimeserieProfile().setName(String.format("%s [Grubbs %.2f]", profile.getName(), 0.05));
					processor.getTimeserieProfile().setDescription(String.format("Outliers removed by Grubbs %.2f - '%s'", 0.05, profile.getName()));
					dbHandler.saveRecord(processor.getTimeserieProfile());
				}
			}
			view.refresh();
			view.deselectAll();
		}
		return Status.OK_STATUS;
	}

}
