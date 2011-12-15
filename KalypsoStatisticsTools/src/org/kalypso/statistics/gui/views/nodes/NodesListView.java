package org.kalypso.statistics.gui.views.nodes;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.viewers.ColumnViewer;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.StyledCellLabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;
import org.kalypso.statistics.gui.AbstractListViewSorter;
import org.kalypso.statistics.gui.AbstractViewFilter;
import org.kalypso.statistics.gui.views.AbstractRecordListViewWithCommandToolbar;
import org.kalypso.statistics.gui.views.IKalypsoStatsRecordViewerSelectable;
import org.kalypso.statistics.gui.views.timeseries.TimeseriesListView;
import org.kalypso.statistics.project.SessionDataProvider;
import org.kalypso.statistics.tools.FormToolkitHelper;
import org.kalypso.statistics.types.CommandToolbarItem;
import org.kalypso.statistics.types.EColumnLabel;
import org.kalypso.statistics.types.EToolBar;
import org.kalypso.statistics.types.data.AbstractStatisticsRecordType;
import org.kalypso.statistics.types.data.NodeProfile;
import org.kalypso.statistics.utils.AppUtils;

public class NodesListView extends AbstractRecordListViewWithCommandToolbar<NodeProfile> {

	public static final String ID = NodesListView.class.getCanonicalName();

	@Override
	protected void loadToolbarItems() {
		final Map<EToolBar, CommandToolbarItem> toolbarItemsMap = getToolbarEnabledItemsMap();
		toolbarItemsMap.put(EToolBar.SELECT_ALL, new CommandToolbarItem("Select all", "Select all"));
		toolbarItemsMap.put(EToolBar.DESELECT_ALL, new CommandToolbarItem("None", "Select none"));
		toolbarItemsMap.put(EToolBar.SEPARATOR, null);
		toolbarItemsMap.put(EToolBar.NEW, new CommandToolbarItem("New", "Create new Node / Station"));
		toolbarItemsMap.put(EToolBar.IMPORT_NODES, new CommandToolbarItem("Import", "Import Nodes / Stations from Shape data"));
		toolbarItemsMap.put(EToolBar.DELETE_SELECTED, new CommandToolbarItem("Delete", "Delete selected Nodes / Stations"));
	}

	@Override
	public ColumnViewer createViewer(final Composite parent, final FormToolkitHelper toolkit) {
		return new TableViewer(parent, SWT.CHECK | SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION | SWT.BORDER);
	}

	@Override
	public AbstractViewFilter createFilter() {
		return new NodesFilter();
	}

	@Override
	public IContentProvider createContentProvider() {
		return new NodesContentProvider();
	}

	@Override
	public StyledCellLabelProvider createLabelProvider() {
		return new NodesListViewLabelProvider(getFieldsList());
	}

	@Override
	public AbstractListViewSorter createSorter() {
		return new NodesListViewSorter(EColumnLabelsNodes.NAME);
	}

	@Override
	protected void loadColumnLabels(final List<EColumnLabel> fieldsList) {
		fieldsList.clear();
		for (EColumnLabel lbl : EColumnLabelsNodes.values()) {
			fieldsList.add(lbl);
		}
	}

	// This will create the columns for the table
	@Override
	protected void createColumns() {
		super.createColumns();
		final TableViewer viewer = (TableViewer) getViewer();
		final Table table = ((TableViewer) getViewer()).getTable();
		// double click opens timeseries view
		viewer.addDoubleClickListener(new DoubleClickListener(TimeseriesListView.ID));
	}

	@Override
	public boolean isApplicableType(final Object object) {
		return object instanceof NodeProfile;
	}

	@Override
	public NodeProfile adaptObject(final Object object) {
		if (isApplicableType(object))
			return (NodeProfile) object;
		return null;
	}

	@Override
	protected List<NodeProfile> getInputSource(final SessionDataProvider sessionProvider) {
		return sessionProvider.getDataProvider().getDbHandlerNodeProfile().getRecords();
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
	public List<NodeProfile> getSelection() {
		final List<NodeProfile> list = new ArrayList<NodeProfile>();
		final TableItem[] items = ((TableViewer) getViewer()).getTable().getItems();
		for (final TableItem item : items) {
			if (item.getChecked())
				list.add((NodeProfile) item.getData());
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
			message[1] = "Permanently delete selected Nodes / Stations?";
			break;
		}
		return message;
	}

	@Override
	public String cannotDeleteRecordsErrMessage(final List<? extends AbstractStatisticsRecordType> records) {
		return null;
	}

	@Override
	public Class<NodeProfile> getRecordType() {
		return NodeProfile.class;
	}

	@Override
	public Class<? extends AbstractStatisticsRecordType> getRecordClass() {
		return NodeProfile.class;
	}

}
