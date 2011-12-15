package org.kalypso.statistics.utils;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.WorkbenchException;
import org.kalypso.chart.ui.view.ChartView;
import org.kalypso.ogc.gml.outline.ViewContentOutline;
import org.kalypso.statistics.db.handler.DBResponse;
import org.kalypso.statistics.gui.chart.TimeseriesChartInput;
import org.kalypso.statistics.gui.views.AbstractRecordListView;
import org.kalypso.statistics.i18n.Messages;
import org.kalypso.statistics.project.SessionDataProvider;
import org.kalypso.statistics.types.data.TimeserieProfile;

public final class WorkbenchUtils {

	private WorkbenchUtils() {
		// DO NOT INSTANTIATE!
	}

	/**
	 * Returns IWorkbenchPart IF FOUND IN CURRENT PERSPECTIVE, or null if no
	 * such part was found
	 * 
	 */
	public static final IWorkbenchPart findPartInCurrentPerspective(final String partID) throws WorkbenchException {
		return findPartInCurrentPerspective(partID, false);
	}

	/**
	 * Returns IWorkbenchPart IF FOUND IN CURRENT PERSPECTIVE, or null if no
	 * such part was found
	 * 
	 */
	public static final IWorkbenchPart findPartInCurrentPerspective(final String partID, final boolean activatePart) {
		final IWorkbenchPage activePage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();

		// pazi, ovaj nalazi view samo ako je u aktivnoj perspektivi!
		final IViewReference viewReference = activePage.findViewReference(partID);

		if (viewReference == null)
			return null;
		return viewReference.getPart(activatePart);
	}

	/**
	 * Returns AbstractEditorView<?> if found in current perspective, or null if
	 * no such part was found
	 * 
	 * @return
	 * @throws WorkbenchException
	 */
	// public static final AbstractRecordEditorView<?> findEditorView() {
	// if (SessionDataProvider.isShutdownInProgress()) {
	// return null;
	// }
	// final IWorkbenchPage activePage =
	// PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
	// final IViewReference[] references = activePage.getViewReferences();
	// for (final IViewReference reference : references) {
	// final IViewPart part = activePage.findView(reference.getId());
	// if (part instanceof AbstractRecordEditorView<?>)
	// return (AbstractRecordEditorView<?>) part;
	// }
	// return null;
	// }

	/**
	 * Returns AbstractListView<?> if found in current perspective, or null if
	 * no such part was found
	 * 
	 * @return
	 * @throws WorkbenchException
	 */
	public static final AbstractRecordListView<?> findListView() {
		if (SessionDataProvider.isShutdownInProgress()) {
			return null;
		}
		final IWorkbenchPage activePage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		final IViewReference[] references = activePage.getViewReferences();
		for (final IViewReference reference : references) {
			final IViewPart part = activePage.findView(reference.getId());
			if (part instanceof AbstractRecordListView<?>)
				return (AbstractRecordListView<?>) part;
		}
		return null;
	}

	public static final IViewPart showView(final String viewID, final boolean maximizeView) throws WorkbenchException {
		final IWorkbenchPage activePage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		if (ChartView.ID.equals(viewID)) {
			activePage.showView(ViewContentOutline.ID);
		}
		final IViewPart view = activePage.showView(viewID);
		final IViewReference viewReference = activePage.findViewReference(viewID);
		activePage.setPartState(viewReference, maximizeView ? IWorkbenchPage.STATE_MAXIMIZED : IWorkbenchPage.STATE_RESTORED);
		return view;
	}

	public static final void restoreView(final String viewID) throws WorkbenchException {
		final IViewPart view = showView(viewID, false);
		final IWorkbenchPage activePage = view.getSite().getWorkbenchWindow().getActivePage();
		final IViewReference viewReference = activePage.findViewReference(viewID);
		activePage.setPartState(viewReference, IWorkbenchPage.STATE_RESTORED);
	}

	public static void showDBResponse(final DBResponse response) {
		Display.getDefault().asyncExec(new Runnable() {
			@Override
			public void run() {
				final Shell shell = Display.getDefault().getActiveShell();
				switch (response.getResponseType()) {
				case OK:
					MessageDialog.openInformation(shell, Messages._INFORMATION, response.getMessage());
					break;
				case WARNING:
					MessageDialog.openWarning(shell, Messages._WARNING, response.getMessage());
					break;
				case ERROR:
					MessageDialog.openError(shell, Messages.Error_GeneralErrorTitle, response.getMessage());
					break;
				}
			}
		});
	}

	public static final void showTimeseries(TimeserieProfile timeserieProfile) throws WorkbenchException {
		final List<TimeserieProfile> profiles = new ArrayList<TimeserieProfile>();
		profiles.add(timeserieProfile);
		showTimeseries(timeserieProfile.getName(), profiles);
	}

	public static final void showTimeseries(String chartTitle, List<TimeserieProfile> timeserieProfiles) throws WorkbenchException {
		final TimeseriesChartInput input = new TimeseriesChartInput(chartTitle, timeserieProfiles);
		final IViewPart view = showView(ChartView.ID, false);
		if (view instanceof ChartView) {
			ChartView cview = (ChartView) view;
			cview.setInput(input);
		}
		view.setFocus();
	}

}
