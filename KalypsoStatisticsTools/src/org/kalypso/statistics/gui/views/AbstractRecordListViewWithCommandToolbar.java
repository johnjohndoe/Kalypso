package org.kalypso.statistics.gui.views;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ParameterizedCommand;
import org.eclipse.core.runtime.ISafeRunnable;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.WorkbenchException;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.chart.ui.view.ChartView;
import org.kalypso.ogc.gml.outline.ViewContentOutline;
import org.kalypso.statistics.gui.views.timeseries.TimeseriesListView;
import org.kalypso.statistics.i18n.Messages;
import org.kalypso.statistics.tools.FormToolkitHelper;
import org.kalypso.statistics.types.CommandToolbarItem;
import org.kalypso.statistics.types.EToolBar;
import org.kalypso.statistics.types.data.AbstractStatisticsRecordType;
import org.kalypso.statistics.types.data.NodeProfile;
import org.kalypso.statistics.types.data.TimeserieProfile;
import org.kalypso.statistics.utils.AppUtils;
import org.kalypso.statistics.utils.WorkbenchUtils;

public abstract class AbstractRecordListViewWithCommandToolbar<T extends AbstractStatisticsRecordType> extends AbstractRecordListView<T> implements
		IKalypsoStatsRecordViewerSelectable<T> {

	protected class DoubleClickListener implements IDoubleClickListener {

		private final String m_viewID;

		public DoubleClickListener(final String viewID) {
			m_viewID = viewID;
		}

		@Override
		public void doubleClick(final DoubleClickEvent event) {
			final ISelection selection = getViewer().getSelection();
			if (selection instanceof IStructuredSelection && !selection.isEmpty()) {
				final Object selectedElement = ((IStructuredSelection) selection).getFirstElement();
				final T object = adaptObject(selectedElement);
				if (object != null) {
					try {
						final IViewPart view = WorkbenchUtils.showView(m_viewID, false);
						if (view instanceof TimeseriesListView) {
							TimeseriesListView tView = (TimeseriesListView) view;
							if (object instanceof NodeProfile) {
								NodeProfile node = (NodeProfile) object;
								tView.setPartName("Timeseries for " + node.getName());
								tView.applyFilterPerNode(node);
								tView.refresh();
							}
						} else if (view instanceof ChartView) {
							WorkbenchUtils.showView(ViewContentOutline.ID, false);
							if (object instanceof TimeserieProfile) {
								WorkbenchUtils.showTimeseries((TimeserieProfile) object);
							}
						}
					} catch (WorkbenchException ex) {
						AppUtils.getLogger().logException(ex);
						throw new RuntimeException(String.format(Messages.Error_InternalErrorRuntimeMessage, m_viewID, ex.getMessage()));
					} finally {
						// setCursorBusy(false);
					}
					SafeRunner.run(new ISafeRunnable() {
						@Override
						public void handleException(final Throwable e) {
							AppUtils.getLogger().logException(e);
						}

						@Override
						public void run() throws Exception {
							// final AbstractRecordEditorView<?> editorView =
							// WorkbenchUtils.findEditorView();
							// if (editorView != null)
							// editorView.loadRecord(object);
						}
					});
				}
			}
		}
	};

	private final Map<EToolBar, CommandToolbarItem> m_toolbarEnabledItemsMap = new LinkedHashMap<EToolBar, CommandToolbarItem>();
	private final Map<EToolBar, ToolItem> m_toolbarItemsMap = new HashMap<EToolBar, ToolItem>();
	private Text m_addText = null;

	@Override
	public void createToolbarSearch(final Composite parent, final FormToolkitHelper toolkit) {
		loadToolbarItems();
		parent.setLayout(new GridLayout(2, true));

		final Composite commandsComposite = toolkit.createComposite(parent, SWT.NONE, 1, 0, 0, 0, 0, true, false);
		commandsComposite.setBackground(parent.getBackground());
		final ToolBar toolBar = toolkit.createToolBar(commandsComposite);
		for (final EToolBar key : getToolbarEnabledItemsMap().keySet()) {
			if (key.equals(EToolBar.SEPARATOR) || key.equals(EToolBar.SEPARATOR2) || key.equals(EToolBar.SEPARATOR3)) {
				toolkit.createToolbarSeparator(toolBar);
			} else {
				final CommandToolbarItem toolBarItem = getToolbarEnabledItemsMap().get(key);
				final ToolItem item = toolkit.createToolItem(toolBar, toolBarItem.getLabel(), toolBarItem.getTooltip(), key.getStatisticsImage());
				// item.setEnabled(false);
				item.addSelectionListener(getToolbarButtonListener(key.getCommandID()));
				getToolbarItemsMap().put(key, item);
			}
		}
		final Composite toolbarComposite = toolkit.createComposite(parent, SWT.NONE, 1, 0, 0, 3, 0, true, false);
		toolbarComposite.setBackground(parent.getBackground());
		super.createToolbarSearch(toolbarComposite, toolkit);
	}

	protected abstract void loadToolbarItems();

	@Override
	public final String getID() {
		return getClass().getSimpleName();
	}

	public Map<EToolBar, CommandToolbarItem> getToolbarEnabledItemsMap() {
		return m_toolbarEnabledItemsMap;
	}

	protected final SelectionListener getToolbarButtonListener(final String commandID) {
		return new SelectionListener() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				handleTextToolbarAction(commandID, null, false);
			}

			@Override
			public void widgetDefaultSelected(final SelectionEvent e) {
			}
		};
	}

	protected final SelectionListener getTextToolbarButtonListener(final String commandID, final Text text, final boolean clearTextOnSuccess) {
		return new SelectionListener() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				handleTextToolbarAction(commandID, text, clearTextOnSuccess);
			}

			@Override
			public void widgetDefaultSelected(final SelectionEvent e) {
			}
		};
	}

	private final void handleTextToolbarAction(final String commandID, final Text text, final boolean clearTextOnSuccess) {
		final ICommandService commandService = (ICommandService) getSite().getService(ICommandService.class);
		final IHandlerService handlerService = (IHandlerService) getSite().getService(IHandlerService.class);
		final Command command = commandService.getCommand(commandID);
		final Map<String, String> params = new HashMap<String, String>();
		params.put("viewID", getID()); //$NON-NLS-1$
		if (text != null)
			params.put("text", text.getText()); //$NON-NLS-1$
		final ParameterizedCommand parameterizedCommand = ParameterizedCommand.generateCommand(command, params);
		try {
			if (parameterizedCommand != null)
				handlerService.executeCommand(parameterizedCommand, null);
			else
				handlerService.executeCommand(commandID, null);
			if (text != null && clearTextOnSuccess)
				text.setText(""); //$NON-NLS-1$
		} catch (final Exception ex) {
			AppUtils.getLogger().logException(ex);
			final String msg = String.format(Messages.Error_InternalErrorRuntimeMessage, commandID, ex.getMessage());
			MessageDialog.openError(Display.getDefault().getActiveShell(), Messages.Error_GeneralErrorTitle,
					String.format(Messages.Error_InternalErrorMessageWithExceptionMessage, msg));
			throw new RuntimeException(msg);
		} finally {
			// setCursorBusy(false);
		}
	}

	public void setAddTextFieldContent(final String text) {
		if (m_addText != null)
			m_addText.setText(text);
	}

	public String getAddTextFieldContent() {
		if (m_addText == null)
			return ""; //$NON-NLS-1$
		return m_addText.getText();
	}

	public final Map<EToolBar, ToolItem> getToolbarItemsMap() {
		return m_toolbarItemsMap;
	}

}
