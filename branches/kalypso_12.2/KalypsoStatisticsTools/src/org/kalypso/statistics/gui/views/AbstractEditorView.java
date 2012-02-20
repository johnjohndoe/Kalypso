package org.kalypso.statistics.gui.views;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ParameterizedCommand;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.IStatusLineManager;
import org.eclipse.jface.action.StatusLineContributionItem;
import org.eclipse.jface.action.StatusLineManager;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.statistics.i18n.Messages;
import org.kalypso.statistics.tools.FormToolkitHelper;
import org.kalypso.statistics.types.CommandToolbarItem;
import org.kalypso.statistics.types.EEditorField;
import org.kalypso.statistics.types.EToolBar;
import org.kalypso.statistics.types.KalypsoStatisticsException;
import org.kalypso.statistics.utils.AppUtils;

public abstract class AbstractEditorView extends ViewPart {

	private final Map<EEditorField, Control> m_pageControls = new HashMap<EEditorField, Control>();
	private final Map<EToolBar, CommandToolbarItem> m_toolbarEnabledItemsMap = new LinkedHashMap<EToolBar, CommandToolbarItem>();
	private final Map<EToolBar, ToolItem> m_toolbarItemsMap = new HashMap<EToolBar, ToolItem>();
	private ToolBar m_toolBar;
	private final int m_minWidth;
	private final int m_minHeight;
	private final boolean m_expandHorizontal;
	private final boolean m_expandVertical;

	private boolean m_tabFolderTraversalEnabled = true;
	private Control m_initialFocusControl;

	public static enum STATUS_BAR_FIELD {
		VIEWER_INFO("abstract.editor.view.info"), //
		SHIPMENT_INFO("editor.shipment.info"), //
		TRKSERIE_INFO("editor.shipment.trkserie");

		private final String m_fieldName;

		private STATUS_BAR_FIELD(final String fieldName) {
			m_fieldName = fieldName;
		}

		public String getFieldName() {
			return m_fieldName;
		}
	}

	public AbstractEditorView() {
		this(600, 500, true, true);
	}

	public AbstractEditorView(final int minWidth, final int minHeight, final boolean expandHorizontal, final boolean expandVertical) {
		m_minWidth = minWidth;
		m_minHeight = minHeight;
		m_expandHorizontal = expandHorizontal;
		m_expandVertical = expandVertical;

		final IPartListener partListener = new IPartListener() {
			@Override
			public void partActivated(final IWorkbenchPart part) {
				PlatformUI.getWorkbench().getDisplay().asyncExec(new Runnable() {
					@Override
					public void run() {
						onPartActivatedDo();
					}
				});
			}

			@Override
			public void partBroughtToTop(final IWorkbenchPart part) {
			}

			@Override
			public void partClosed(final IWorkbenchPart part) {
			}

			@Override
			public void partDeactivated(final IWorkbenchPart part) {
			}

			@Override
			public void partOpened(final IWorkbenchPart part) {
			}
		};
		final IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		if (page != null)
			page.addPartListener(partListener);
	}

	protected void onPartActivatedDo() {
	}

	public final Map<EEditorField, Control> getPageControls() {
		return m_pageControls;
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
		final FormToolkitHelper toolkit = new FormToolkitHelper(parent.getDisplay());
		final ScrolledComposite scroll = new ScrolledComposite(parent, SWT.H_SCROLL);
		final Composite holder = toolkit.createComposite(scroll, SWT.NONE, 1, 0, 0, 0, 0, true, true);
		scroll.setContent(holder);
		scroll.setMinSize(m_minWidth, m_minHeight);
		scroll.setExpandHorizontal(m_expandHorizontal);
		scroll.setExpandVertical(m_expandVertical);
		final Composite toolbarHolderComposite = toolkit.createComponentHolderComposite(holder, SWT.NONE, 1, false, 1);
		final Composite dataHolderComposite = toolkit.createComponentHolderComposite(holder, SWT.NONE, 1, true, 1);
		m_toolBar = toolkit.createToolBar(toolbarHolderComposite);
		for (final EToolBar key : getToolbarEnabledItemsMap().keySet()) {
			if (key.equals(EToolBar.SEPARATOR) || key.equals(EToolBar.SEPARATOR2) || key.equals(EToolBar.SEPARATOR3)) {
				toolkit.createToolbarSeparator(m_toolBar);
			} else {
				final CommandToolbarItem toolBarItem = getToolbarEnabledItemsMap().get(key);
				final ToolItem item = toolkit.createToolItem(m_toolBar, toolBarItem.getLabel(), toolBarItem.getTooltip(), key.getStatisticsImage());
				item.setEnabled(false);
				item.addSelectionListener(getToolbarButtonListener(key.getCommandID()));
				getToolbarItemsMap().put(key, item);
			}
		}
		try {
			createPartControlContent(dataHolderComposite, toolkit);
			getFancySaveControl(dataHolderComposite, toolkit);

			final List<Control> tabList = new ArrayList<Control>();
			for (final Control child : dataHolderComposite.getChildren()) {
				tabList.add(child);
			}
			setInitialFocusControl(dataHolderComposite.getChildren()[0]);
			tabList.add(getInitialFocusControl());
			dataHolderComposite.setTabList(tabList.toArray(new Control[tabList.size()]));

		} catch (final KalypsoStatisticsException e) {
			MessageDialog.openError(parent.getShell(), Messages.Error_InternalErrorTitle,
					String.format(Messages.Error_InternalErrorMessageWithExceptionMessage, e.getMessage()));
			// TODO quit?
			AppUtils.getLogger().logException(e);
		}
		// Display.getDefault().asyncExec(new Runnable() {
		// @Override
		// public void run() {
		// // updateToolbar();
		// }
		// });
	}

	public void updateStatusBar() {
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

	protected void getFancySaveControl(final Composite parent, final FormToolkitHelper toolkit) {
	}

	protected abstract void createPartControlContent(final Composite holder, final FormToolkitHelper toolkit) throws KalypsoStatisticsException;

	@Override
	public void setFocus() {
		updateStatusBar();
	}

	protected final boolean isThisViewVisible() {
		return getSite().getPage().isPartVisible(this);
	}

	protected abstract void updateToolbar();

	protected final SelectionListener getToolbarButtonListener(final String commandID) {
		return new SelectionListener() {

			@Override
			public void widgetSelected(final SelectionEvent e) {
				final ICommandService commandService = (ICommandService) getSite().getService(ICommandService.class);
				final IHandlerService handlerService = (IHandlerService) getSite().getService(IHandlerService.class);
				final Command command = commandService.getCommand(commandID);
				final Map<String, String> params = new HashMap<String, String>();
				params.put("viewID", getID()); //$NON-NLS-1$
				final ParameterizedCommand parameterizedCommand = ParameterizedCommand.generateCommand(command, params);
				try {
					if (parameterizedCommand != null)
						handlerService.executeCommand(parameterizedCommand, null);
					else
						handlerService.executeCommand(commandID, null);
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

			@Override
			public void widgetDefaultSelected(final SelectionEvent e) {
			}
		};
	}

	public final boolean executeCommand(final String commandID) {
		final IHandlerService handlerService = (IHandlerService) getSite().getService(IHandlerService.class);
		try {
			final Object response = handlerService.executeCommand(commandID, null);
			if (response instanceof Boolean) {
				return ((Boolean) response).booleanValue();
			}
		} catch (final Exception e) {
			AppUtils.getLogger().logException(e);
		}
		return false;
	}

	public final String getID() {
		return getClass().getSimpleName();
	}

	public final Map<EToolBar, CommandToolbarItem> getToolbarEnabledItemsMap() {
		return m_toolbarEnabledItemsMap;
	}

	public final Map<EToolBar, ToolItem> getToolbarItemsMap() {
		return m_toolbarItemsMap;
	}

	public ToolBar getToolBar() {
		return m_toolBar;
	}

	public void setTabFolderTraversalEnabled(final boolean tabFolderTraversalEnabled) {
		m_tabFolderTraversalEnabled = tabFolderTraversalEnabled;
	}

	public boolean isTabFolderTraversalEnabled() {
		return m_tabFolderTraversalEnabled;
	}

	private void setInitialFocusControl(final Control initialFocusControl) {
		m_initialFocusControl = initialFocusControl;
	}

	public Control getInitialFocusControl() {
		return m_initialFocusControl;
	}

}
