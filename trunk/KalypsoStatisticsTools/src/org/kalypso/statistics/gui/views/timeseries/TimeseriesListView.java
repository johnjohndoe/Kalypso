package org.kalypso.statistics.gui.views.timeseries;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.viewers.ColumnViewer;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.StyledCellLabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TableItem;
import org.kalypso.chart.ui.view.ChartView;
import org.kalypso.statistics.gui.AbstractListViewSorter;
import org.kalypso.statistics.gui.AbstractViewFilter;
import org.kalypso.statistics.gui.views.AbstractRecordListViewWithCommandToolbar;
import org.kalypso.statistics.gui.views.GeneralContentProvider;
import org.kalypso.statistics.gui.views.IKalypsoStatsRecordViewerSelectable;
import org.kalypso.statistics.project.SessionDataProvider;
import org.kalypso.statistics.tools.FormToolkitHelper;
import org.kalypso.statistics.types.CommandToolbarItem;
import org.kalypso.statistics.types.EColumnLabel;
import org.kalypso.statistics.types.EToolBar;
import org.kalypso.statistics.types.data.AbstractStatisticsRecordType;
import org.kalypso.statistics.types.data.NodeProfile;
import org.kalypso.statistics.types.data.TimeserieProfile;
import org.kalypso.statistics.utils.AppUtils;

public class TimeseriesListView extends AbstractRecordListViewWithCommandToolbar<TimeserieProfile> {

	public static final String ID = TimeseriesListView.class.getCanonicalName();

	@Override
	protected void loadToolbarItems() {
		final Map<EToolBar, CommandToolbarItem> toolbarItemsMap = getToolbarEnabledItemsMap();
		toolbarItemsMap.put(EToolBar.SELECT_ALL, new CommandToolbarItem("Select all", "Select all"));
		toolbarItemsMap.put(EToolBar.DESELECT_ALL, new CommandToolbarItem("None", "Select none"));
		toolbarItemsMap.put(EToolBar.SEPARATOR, null);
		toolbarItemsMap.put(EToolBar.DISPLAY_CHART_TIMESERIES, new CommandToolbarItem("Chart", "Display selected timeseries"));
		toolbarItemsMap.put(EToolBar.IMPORT_TIMESERIES, new CommandToolbarItem("Import", "Import Timeseries from different formats"));
		toolbarItemsMap.put(EToolBar.PREPROCESS_DISCHARGE_TIMESERIES, new CommandToolbarItem("Preprocess", "Preprocess Selected Discharge Timeseries"));
		toolbarItemsMap.put(EToolBar.CREATE_EXTREMS_TIMESERIES, new CommandToolbarItem("Extrems", "Create Extrems Timeseries"));
		toolbarItemsMap.put(EToolBar.TREND_ADJUSTMENT, new CommandToolbarItem("Trend", "Trend adjustment"));
		toolbarItemsMap.put(EToolBar.GRUBBS, new CommandToolbarItem("Grubbs", "Grubbs test"));
		toolbarItemsMap.put(EToolBar.DELETE_SELECTED, new CommandToolbarItem("Delete", "Delete selected Timeseries"));
	}

	@Override
	public ColumnViewer createViewer(final Composite parent, final FormToolkitHelper toolkit) {
		return new TableViewer(parent, SWT.CHECK | SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION | SWT.BORDER);
	}

	@Override
	public AbstractViewFilter createFilter() {
		return new TimeseriesViewFilter();
	}

	@Override
	public IContentProvider createContentProvider() {
		return new GeneralContentProvider<TimeserieProfile>();
	}

	@Override
	public StyledCellLabelProvider createLabelProvider() {
		return new TimeseriesListViewLabelProvider(getFieldsList());
	}

	@Override
	public AbstractListViewSorter createSorter() {
		return new TimeseriesListViewSorter(EColumnLabelsTimeseries.NAME);
	}

	@Override
	protected void loadColumnLabels(final List<EColumnLabel> fieldsList) {
		for (final EColumnLabelsTimeseries field : EColumnLabelsTimeseries.values()) {
			fieldsList.add(field);
		}
	}

	// This will create the columns for the table
	@Override
	protected void createColumns() {
		super.createColumns();
		final TableViewer viewer = (TableViewer) getViewer();
		// double click opens shipment editor
		viewer.addDoubleClickListener(new DoubleClickListener(ChartView.ID));
	}

	@Override
	public boolean isApplicableType(final Object object) {
		return object instanceof TimeserieProfile;
	}

	@Override
	public TimeserieProfile adaptObject(final Object object) {
		if (isApplicableType(object))
			return (TimeserieProfile) object;
		return null;
	}

	@Override
	protected List<TimeserieProfile> getInputSource(final SessionDataProvider sessionProvider) {
		return sessionProvider.getDataProvider().getTimeseries();
	}

	@Override
	public void selectAll() {
		setSelectionAll(true);
	}

	@Override
	public void deselectAll() {
		setSelectionAll(false);
	}

	@Override
	public List<TimeserieProfile> getSelection() {
		final List<TimeserieProfile> list = new ArrayList<TimeserieProfile>();
		final TableItem[] items = ((TableViewer) getViewer()).getTable().getItems();
		for (final TableItem item : items) {
			if (item.getChecked())
				list.add((TimeserieProfile) item.getData());
		}
		return list;
	}

	private void setSelectionAll(final boolean selected) {
		final TableItem[] items = ((TableViewer) getViewer()).getTable().getItems();
		for (final TableItem item : items) {
			item.setChecked(selected);
		}
	}

	@Override
	public String[] getCustomMessage(final IKalypsoStatsRecordViewerSelectable.TYPE type) {
		final String[] message = new String[2];
		switch (type) {
		case DELETE:
			message[0] = AppUtils.APPLICATION_TITLE;
			message[1] = "Permanently delete selected timeseries?";
			break;
		}
		return message;
	}

	@Override
	public String cannotDeleteRecordsErrMessage(final List<? extends AbstractStatisticsRecordType> records) {
		return null;
	}

	@Override
	public Class<TimeserieProfile> getRecordType() {
		return TimeserieProfile.class;
	}

	@Override
	public Class<? extends AbstractStatisticsRecordType> getRecordClass() {
		return TimeserieProfile.class;
	}

	public void applyFilterPerNode(NodeProfile node) {
		if (node == null) {
			// remove filters
			SessionDataProvider.getInstance().getDataProvider().getDbHandlerTimeseriesProfile().reload();
		} else {
			// apply node filter
			SessionDataProvider.getInstance().getDataProvider().getDbHandlerTimeseriesProfile().loadRecordsForNode(node);
		}
	}

}
