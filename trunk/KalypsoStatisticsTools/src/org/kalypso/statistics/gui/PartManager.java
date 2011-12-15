package org.kalypso.statistics.gui;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.kalypso.statistics.db.datasource.DataSourceManager;
import org.kalypso.statistics.gui.views.IStatisticsRecordListView;
import org.kalypso.statistics.gui.views.IStatisticsRecordLoadListener;
import org.kalypso.statistics.types.data.AbstractStatisticsRecordType;
import org.kalypso.statistics.utils.AppUtils;
import org.kalypso.statistics.utils.WorkbenchUtils;

public class PartManager implements IPartListener {

	private static PartManager INSTANCE = null;

	private IWorkbenchPart m_activePart = null;

	private final List<IWorkbenchPart> m_partList = new ArrayList<IWorkbenchPart>();
	private final Map<String, List<IStatisticsRecordLoadListener<? extends AbstractStatisticsRecordType>>> m_pendingListeners = new HashMap<String, List<IStatisticsRecordLoadListener<? extends AbstractStatisticsRecordType>>>();

	private PartManager() {
		// do not instantiate
	}

	public static PartManager getInstance() {
		if (INSTANCE == null) {
			INSTANCE = new PartManager();
		}
		return INSTANCE;
	}

	public void initialize() {
		Display.getDefault().asyncExec(new Runnable() {
			@Override
			public void run() {
				try {
					final IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
					if (window != null) {
						window.getActivePage().addPartListener(getInstance());
						m_activePart = window.getActivePage().getActivePart();
						partActivated(m_activePart);
						// we are not using editors, just views...
						for (final IViewReference ref : window.getActivePage().getViewReferences()) {
							final IWorkbenchPart part = WorkbenchUtils.findPartInCurrentPerspective(ref.getId(), false);
							if (part != null)
								registerPart(part);
						}

						// register opened parts
						for (final IViewReference partReference : window.getActivePage().getViewReferences()) {
							final IWorkbenchPart part = partReference.getView(false);
							if (part != null)
								registerPart(part);
						}
						for (final IEditorReference partReference : window.getActivePage().getEditorReferences()) {
							final IWorkbenchPart part = partReference.getEditor(false);
							if (part != null)
								registerPart(part);
						}

						if (DataSourceManager.getDataSource().isDataSourceErrorOccured()) {
							MessageDialog.openError(Display.getDefault().getActiveShell(), "Error", "SeriousDatabaseErrorMsg");
						}
					}
				} catch (final Exception e) {
					AppUtils.getLogger().logException(e);
				}
			}
		});
	}

	public IWorkbenchPart getWorkbenchPart(final String partID) {
		for (final IWorkbenchPart part : m_partList) {
			if (part.getClass().getSimpleName().equals(partID))
				return part;
		}
		return null;
	}

	public IWorkbenchPart[] getWorkbenchParts() {
		return m_partList.toArray(new IWorkbenchPart[m_partList.size()]);
	}

	public String getPerspectiveID(final String viewID) {
		for (final IWorkbenchPart part : m_partList) {
			if (part.getSite().getId().equals(viewID))
				return part.getSite().getPage().getPerspective().getId();
		}
		return null;
	}

	public IWorkbenchPart getActivePart() {
		if (m_activePart == null) {
			m_activePart = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActivePart();
		}
		return m_activePart;
	}

	public void refreshActiveView() {
		// if (m_activePart instanceof AbstractRecordEditorView<?>) {
		// final AbstractRecordEditorView<?> view =
		// (AbstractRecordEditorView<?>) m_activePart;
		// view.onPartActivatedFirstTime();
		// }
	}

	@Override
	public void partActivated(final IWorkbenchPart part) {
		registerPart(part);
		m_activePart = part;
	}

	@Override
	public void partBroughtToTop(final IWorkbenchPart part) {
		// nothing to do
	}

	@Override
	public void partClosed(final IWorkbenchPart part) {
		unregisterPart(part);
	}

	@Override
	public void partDeactivated(final IWorkbenchPart part) {
		// nothing to do
	}

	@Override
	public void partOpened(final IWorkbenchPart part) {
		registerPart(part);
	}

	private void registerPart(final IWorkbenchPart part) {
		synchronized (this) {
			if (!m_partList.contains(part)) {
				m_partList.add(part);
				if (part instanceof IViewPart) {
					final IViewPart view = (IViewPart) part;
					if (part instanceof IStatisticsRecordListView<?>) {
						final IStatisticsRecordListView<?> myPart = (IStatisticsRecordListView<?>) view;
						myPart.setInputSource();
						System.out.println("Part registered: " + myPart.getID());
					}
				}
			}
		}
	}

	private synchronized void unregisterPart(final IWorkbenchPart part) {
		m_partList.remove(part);
	}

	private void _internalRegisterPendingListeners(final IStatisticsRecordListView<? extends AbstractStatisticsRecordType> view) {
		if (m_pendingListeners.containsKey(view.getID())) {
			Display.getDefault().asyncExec(new Runnable() {
				@Override
				public void run() {
					final List<IStatisticsRecordLoadListener<? extends AbstractStatisticsRecordType>> list = m_pendingListeners.get(view.getID());
					for (final IStatisticsRecordLoadListener listener : list) {
						view.addCustomSelectionListener(listener);
					}
				}
			});
		}
	}

	private void _internalPutListenerOnPending(final String listPartID, final IStatisticsRecordLoadListener<? extends AbstractStatisticsRecordType> listener) {
		if (m_pendingListeners.containsKey(listPartID)) {
			final List<IStatisticsRecordLoadListener<? extends AbstractStatisticsRecordType>> list = m_pendingListeners.get(listPartID);
			list.add(listener);
		} else {
			final List<IStatisticsRecordLoadListener<? extends AbstractStatisticsRecordType>> list = new ArrayList<IStatisticsRecordLoadListener<? extends AbstractStatisticsRecordType>>();
			list.add(listener);
			m_pendingListeners.put(listPartID, list);
		}
	}

	public void registerCustomSelectionListener(final String selectionProducerPartID,
			final IStatisticsRecordLoadListener<? extends AbstractStatisticsRecordType> listener) {
		if (selectionProducerPartID == null)
			return;
		for (final IWorkbenchPart part : m_partList) {
			if (part instanceof IStatisticsRecordListView<?>) {
				final IStatisticsRecordListView<?> view = (IStatisticsRecordListView<?>) part;
				if (selectionProducerPartID.equals(view.getID())) {
					_internalPutListenerOnPending(selectionProducerPartID, listener);
					_internalRegisterPendingListeners(view);
					return;
				}
			}
		}
		// part not registered yet, put listener on pending
		_internalPutListenerOnPending(selectionProducerPartID, listener);
	}

	public void refreshRecordViewers() {
		final IWorkbenchPart[] parts = getWorkbenchParts();
		for (final IWorkbenchPart part : parts) {
			if (part instanceof IStatisticsRecordListView) {
				final IStatisticsRecordListView recordListView = (IStatisticsRecordListView) part;
				recordListView.refresh();
			}
		}
	}

	public void refreshRecordViewers(final Class<? extends AbstractStatisticsRecordType> clazz) {
		refreshRecordViewers();
		GlobalListenersManager.getInstance().informListeners(clazz);
	}
}
