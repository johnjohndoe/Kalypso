package org.kalypso.statistics.gui.views;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.IStatusLineManager;
import org.eclipse.jface.action.StatusLineContributionItem;
import org.eclipse.jface.action.StatusLineManager;
import org.eclipse.jface.viewers.ColumnViewer;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StyledCellLabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.TreeViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.statistics.gui.AbstractListViewSorter;
import org.kalypso.statistics.gui.AbstractViewFilter;
import org.kalypso.statistics.project.SessionDataProvider;
import org.kalypso.statistics.tools.FormToolkitHelper;
import org.kalypso.statistics.types.EColumnLabel;
import org.kalypso.statistics.types.EStatisticsImage;
import org.kalypso.statistics.types.data.AbstractStatisticsRecordType;
import org.kalypso.statistics.utils.AppUtils;

public abstract class AbstractRecordListView<T extends AbstractStatisticsRecordType> extends ViewPart implements IStatisticsRecordListView<T> {
	private ColumnViewer m_viewer;
	private AbstractViewFilter m_filter;
	private AbstractListViewSorter m_sorter;
	private StyledCellLabelProvider m_labelProvider;

	private final List<EColumnLabel> m_fieldsList = new ArrayList<EColumnLabel>();
	private final List<IStatisticsRecordLoadListener<T>> m_customListenersList = new ArrayList<IStatisticsRecordLoadListener<T>>();
	private boolean m_toolbarSearchEnabled = true;
	private String m_toolbarSearchLabel = "Find";
	private boolean m_isEditingSupportEnabled = false;

	public static enum STATUS_BAR_FIELD {
		VIEWER_INFO("abstract.list.view.info"), //
		TRKSERIE_INFO("editor.shipment.trkserie");

		private final String m_fieldName;

		private STATUS_BAR_FIELD(final String fieldName) {
			m_fieldName = fieldName;
		}

		public String getFieldName() {
			return m_fieldName;
		}
	}

	@Override
	public String getID() {
		return getClass().getSimpleName();
	}

	@Override
	public void init(final IViewSite site) throws PartInitException {
		final IStatusLineManager statusLineManager = site.getActionBars().getStatusLineManager();
		for (final STATUS_BAR_FIELD itemID : STATUS_BAR_FIELD.values()) {
			final StatusLineContributionItem item = new StatusLineContributionItem(itemID.getFieldName(), StatusLineContributionItem.CALC_TRUE_WIDTH);
			statusLineManager.appendToGroup(StatusLineManager.END_GROUP, item);
			// statusLineManager.add(item);
		}
		super.init(site);
	}

	@Override
	public final void createPartControl(final Composite parent) {
		final GridLayout layout = new GridLayout(1, false);
		layout.marginWidth = 5;
		layout.marginHeight = 0;
		layout.horizontalSpacing = 5;
		layout.verticalSpacing = 1;
		parent.setLayout(layout);
		final FormToolkitHelper toolkit = new FormToolkitHelper(parent.getDisplay());
		final Composite toolbarComposite = toolkit.createComposite(parent, SWT.NONE, 1, 0, 0, 3, 0, true, false);
		toolbarComposite.setBackground(parent.getBackground());
		createToolbarSearch(toolbarComposite, toolkit);

		m_viewer = createViewer(parent, toolkit);
		loadColumnLabels(m_fieldsList);
		createColumns();
		m_viewer.setContentProvider(createContentProvider());

		m_labelProvider = createLabelProvider();
		m_sorter = createSorter();
		m_filter = createFilter();

		m_viewer.setLabelProvider(m_labelProvider);
		m_viewer.setSorter(m_sorter);
		m_viewer.addFilter(m_filter);

		// Layout the viewer
		m_viewer.getControl().setLayoutData(toolkit.createGridData(3, 1, true, true));

		createCustomListeners();
	}

	protected abstract void loadColumnLabels(final List<EColumnLabel> fieldsList);

	protected void enableToolbarSearch(final boolean enabled) {
		m_toolbarSearchEnabled = enabled;
	}

	protected void setCustomToolbarSearchLabel(final String label) {
		m_toolbarSearchLabel = label;
	}

	protected void createToolbarSearch(final Composite parent, final FormToolkitHelper toolkit) {
		if (!m_toolbarSearchEnabled)
			return;
		final GridLayout layout = new GridLayout(3, false);
		parent.setLayout(layout);
		toolkit.createLabel(parent, m_toolbarSearchLabel + ": ");
		final Text searchText = toolkit.createText(parent, SWT.BORDER | SWT.SEARCH, true);
		searchText.addKeyListener(new KeyAdapter() {
			@Override
			public void keyReleased(final KeyEvent ke) {
				if (ke.keyCode == SWT.ESC) {
					searchText.setText(""); //$NON-NLS-1$
					getFilter().setSearchText(""); //$NON-NLS-1$
					getViewer().refresh();
				} else {
					getFilter().setSearchText(searchText.getText());
					getViewer().refresh();
				}
			}
		});
		final ToolBar toolBar = toolkit.createToolBar(parent, false);
		final ToolItem item = toolkit.createToolItem(toolBar, null, "Show all, without filtering", EStatisticsImage.ICON_REMOVEFILTER_16);
		item.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				searchText.setText(""); //$NON-NLS-1$
				getFilter().setSearchText(""); //$NON-NLS-1$
				getViewer().refresh();
			}

			@Override
			public void widgetDefaultSelected(final SelectionEvent e) {
			}
		});
	}

	/**
	 * Enables or disables editing support (disabled by default). If enabled,
	 * method <code>getEditingSupport</code> must be implemented by ancestor
	 * class.
	 */
	protected void setEditingSupportEnabled(final boolean enabled) {
		m_isEditingSupportEnabled = enabled;
	}

	/**
	 * Ancestors using editing support must override this method
	 */
	protected EditingSupport setEditingSupport(final ColumnViewer viewer, final EColumnLabel columnLabel) {
		if (m_isEditingSupportEnabled)
			throw new UnsupportedOperationException("Method not implemented by ancestor.");
		return null;
	}

	/**
	 * This will create the columns for the table. <br>
	 * <b>It supports TableViewer and TreeViewer viewers only!</b> <br>
	 * Ancestors with other implementations of <b>ColumnViewer</b> should
	 * override this method.
	 */
	protected void createColumns() {
		final ColumnViewer columnViewer = getViewer();
		if (columnViewer instanceof TableViewer) {
			createTableViewerColumns();
			return;
		}
		if (columnViewer instanceof TreeViewer) {
			createTreeViewerColumns();
			return;
		}
	}

	private void createTableViewerColumns() {
		final TableViewer viewer = (TableViewer) getViewer();
		for (final EColumnLabel field : getFieldsList()) {
			final TableViewerColumn viewerColumn = new TableViewerColumn(viewer, SWT.NONE);
			final TableColumn column = viewerColumn.getColumn();
			column.setText(field.getLabel().replaceAll("\\s+", " "));
			column.setWidth(field.getWidth());
			column.setResizable(true);
			column.setMoveable(true);
			// Setting the right sorter
			column.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(final SelectionEvent e) {
					getSorter().setColumn(field);
					int dir = viewer.getTable().getSortDirection();
					if (viewer.getTable().getSortColumn() == column) {
						dir = dir == SWT.UP ? SWT.DOWN : SWT.UP;
					} else {
						dir = SWT.DOWN;
					}
					viewer.getTable().setSortDirection(dir);
					viewer.getTable().setSortColumn(column);
					viewer.refresh();
				}
			});
			if (m_isEditingSupportEnabled)
				viewerColumn.setEditingSupport(setEditingSupport(viewer, field));
		}
		final Table table = viewer.getTable();
		table.setHeaderVisible(true);
		table.setLinesVisible(true);
	}

	private void createTreeViewerColumns() {
		final TreeViewer viewer = (TreeViewer) getViewer();
		for (final EColumnLabel field : getFieldsList()) {
			final TreeViewerColumn viewerColumn = new TreeViewerColumn(viewer, SWT.NONE);
			final TreeColumn column = viewerColumn.getColumn();
			column.setText(field.getLabel().replaceAll("\\s+", " "));
			column.setWidth(field.getWidth());
			column.setResizable(true);
			column.setMoveable(true);
			// Setting the right sorter
			column.addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(final SelectionEvent e) {
					getSorter().setColumn(field);
					int dir = viewer.getTree().getSortDirection();
					if (viewer.getTree().getSortColumn() == column) {
						dir = dir == SWT.UP ? SWT.DOWN : SWT.UP;
					} else {
						dir = SWT.DOWN;
					}
					viewer.getTree().setSortDirection(dir);
					viewer.getTree().setSortColumn(column);
					viewer.refresh();
				}
			});
			if (m_isEditingSupportEnabled)
				viewerColumn.setEditingSupport(setEditingSupport(viewer, field));
		}
		final Tree tree = viewer.getTree();
		tree.setHeaderVisible(true);
		tree.setLinesVisible(true);
	}

	public abstract IContentProvider createContentProvider();

	public abstract ColumnViewer createViewer(final Composite parent, final FormToolkitHelper toolkit);

	public final ColumnViewer getViewer() {
		return m_viewer;
	}

	public abstract AbstractViewFilter createFilter();

	public final AbstractViewFilter getFilter() {
		return m_filter;
	}

	public abstract AbstractListViewSorter createSorter();

	public final AbstractListViewSorter getSorter() {
		return m_sorter;
	}

	public abstract StyledCellLabelProvider createLabelProvider();

	public final StyledCellLabelProvider getLabelProvider() {
		return m_labelProvider;
	}

	@Override
	public void addCustomSelectionListener(final IStatisticsRecordLoadListener<T> listener) {
		if (!m_customListenersList.contains(listener)) {
			m_customListenersList.add(listener);
			// System.out.println(String.format("Listener '%s' activated for view '%s'.",
			// listener.getClass().getSimpleName(), getID()));
		}
	}

	@Override
	public void removeCustomSelectionListener(final IStatisticsRecordLoadListener<T> listener) {
		m_customListenersList.remove(listener);
	}

	@Override
	public void setInputSource() {
		final SessionDataProvider sessionProvider = SessionDataProvider.getInstance();
		m_viewer.setInput(getInputSource(sessionProvider));
	}

	@Override
	public void refresh() {
		getViewer().refresh();
	}

	public abstract Class<? extends AbstractStatisticsRecordType> getRecordClass();

	/**
	 * Passing the focus request to the viewer's control.
	 */
	@Override
	public void setFocus() {
		getViewer().getControl().setFocus();
		final IStatusLineManager statusLineManager = getViewSite().getActionBars().getStatusLineManager();
		final IContributionItem[] items = statusLineManager.getItems();
		for (final IContributionItem item : items) {
			if (STATUS_BAR_FIELD.VIEWER_INFO.getFieldName().equals(item.getId())) {
				final StatusLineContributionItem statusItem = (StatusLineContributionItem) item;
				statusItem.setText(String.format("%s - %s", AppUtils.APPLICATION_TITLE, getTitle())); //$NON-NLS-1$
				statusLineManager.update(true);
			}
		}
	}

	protected abstract List<T> getInputSource(final SessionDataProvider sessionProvider);

	@Override
	public abstract boolean isApplicableType(final Object object);

	/**
	 * Adapts the given object to T
	 * 
	 * @param object
	 *            , an object to adapt
	 * @return adapted object of type T, or <code>null</code> if the object
	 *         cannot be adapted
	 */
	public abstract T adaptObject(final Object object);

	private void createCustomListeners() {
		getViewer().getControl().addKeyListener(new KeyListener() {

			@Override
			public void keyReleased(final KeyEvent e) {
				if (e.keyCode == SWT.CR || e.keyCode == SWT.KEYPAD_CR) {
					final ISelection selection = m_viewer.getSelection();
					if (selection instanceof IStructuredSelection && !selection.isEmpty()) {
						final Object selectedElement = ((IStructuredSelection) selection).getFirstElement();
						final T object = adaptObject(selectedElement);
						if (object != null) {
							for (final IStatisticsRecordLoadListener<T> item : m_customListenersList) {
								SafeRunner.run(new ISafeRunnable() {
									@Override
									public void handleException(final Throwable e) {
										AppUtils.getLogger().logException(e);
									}

									@Override
									public void run() throws Exception {
										item.loadRecord(object);
									}
								});
							}
						}
					}
				}
			}

			@Override
			public void keyPressed(final KeyEvent e) {
			}
		});

		getViewer().addDoubleClickListener(new IDoubleClickListener() {

			@Override
			public void doubleClick(final DoubleClickEvent event) {
				final ISelection selection = m_viewer.getSelection();
				if (selection instanceof IStructuredSelection && !selection.isEmpty()) {
					final Object selectedElement = ((IStructuredSelection) selection).getFirstElement();
					final T object = adaptObject(selectedElement);
					if (object != null) {
						for (final IStatisticsRecordLoadListener<T> item : m_customListenersList) {
							SafeRunner.run(new ISafeRunnable() {
								@Override
								public void handleException(final Throwable e) {
									AppUtils.getLogger().logException(e);
								}

								@Override
								public void run() throws Exception {
									item.loadRecord(object);
								}
							});
						}
					}
				}
			}
		});
	}

	public List<EColumnLabel> getFieldsList() {
		return m_fieldsList;
	}

}
