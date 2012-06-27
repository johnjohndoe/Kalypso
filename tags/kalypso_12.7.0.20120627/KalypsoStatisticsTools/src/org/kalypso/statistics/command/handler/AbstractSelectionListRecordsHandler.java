package org.kalypso.statistics.command.handler;

import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.IWorkbenchPart;
import org.kalypso.statistics.db.handler.AbstractRecordDBHandler;
import org.kalypso.statistics.db.handler.DBResponse;
import org.kalypso.statistics.db.handler.DBResponse.EDBResponseType;
import org.kalypso.statistics.gui.PartManager;
import org.kalypso.statistics.gui.views.IKalypsoStatsRecordViewerSelectable;
import org.kalypso.statistics.project.SessionDataProvider;
import org.kalypso.statistics.types.EToolBar;
import org.kalypso.statistics.types.data.AbstractStatisticsRecordType;
import org.kalypso.statistics.utils.AppUtils;
import org.kalypso.statistics.utils.WorkbenchUtils;

public abstract class AbstractSelectionListRecordsHandler extends AbstractHandler implements IHandler {

	private boolean m_selectionCheckOverride = false;

	@Override
	public final Object execute(final ExecutionEvent event) throws ExecutionException {
		final IWorkbenchPart activePart = PartManager.getInstance().getActivePart();
		if (activePart != null && activePart instanceof IKalypsoStatsRecordViewerSelectable<?>) {
			final IKalypsoStatsRecordViewerSelectable<?> view = (IKalypsoStatsRecordViewerSelectable<?>) activePart;

			// check if command should be active
			boolean activated = false;
			for (final EToolBar value : view.getToolbarEnabledItemsMap().keySet()) {
				if (value.getCommandID().equals(getID())) {
					final ToolItem item = view.getToolbarItemsMap().get(value);
					activated = item.getEnabled();
					break;
				}
			}
			if (!activated)
				return Boolean.FALSE;

			final List<? extends AbstractStatisticsRecordType> selection = view.getSelection();
			if (!isSelectionCheckIgnored()) {
				if (selection == null || selection.size() == 0) {
					MessageDialog.openWarning(Display.getDefault().getActiveShell(), AppUtils.APPLICATION_TITLE, "No selection was made");
					return Boolean.FALSE;
				}
			}
			final boolean isOK = viewerPreProcessAction(view, selection);
			if (isOK) {
				final SessionDataProvider sessionProvider = SessionDataProvider.getInstance();
				final Class<? extends AbstractStatisticsRecordType> recordType = view.getRecordType();
				final AbstractRecordDBHandler<? extends AbstractStatisticsRecordType> dbHandler = sessionProvider.getDataProvider().getRecordDBHandler(
						recordType);
				if (dbHandler == null) {
					DBResponse dbResponse = new DBResponse(EDBResponseType.ERROR, "Unknown data type.");
					WorkbenchUtils.showDBResponse(dbResponse);
				} else {
					final DBResponse response = executeCommand(dbHandler, selection);
					if (response.isOK()) {
						viewerPostProcessAction(view);
						// update applicable list viewers
						PartManager.getInstance().refreshRecordViewers(recordType);
						return Boolean.TRUE;
					} else {
						WorkbenchUtils.showDBResponse(response);
					}
				}
			}
		}
		return Boolean.FALSE;
	}

	protected abstract String getID();

	protected abstract DBResponse executeCommand(final AbstractRecordDBHandler<? extends AbstractStatisticsRecordType> dbHandler,
			final List<? extends AbstractStatisticsRecordType> records);

	/**
	 * returns true if command can continue, false otherwise; used typically to
	 * check if all the necessary fields are filled in the editor
	 * 
	 * @param selection
	 * 
	 * @param editor
	 * @return
	 */
	protected boolean viewerPreProcessAction(final IKalypsoStatsRecordViewerSelectable<? extends AbstractStatisticsRecordType> view,
			final List<? extends AbstractStatisticsRecordType> selection) {
		return true;
	}

	protected void viewerPostProcessAction(final IKalypsoStatsRecordViewerSelectable<? extends AbstractStatisticsRecordType> view) {
	}

	protected void setIgnoreSelectionCheckOverride(final boolean selectionCheckOverride) {
		m_selectionCheckOverride = selectionCheckOverride;
	}

	protected boolean isSelectionCheckIgnored() {
		return m_selectionCheckOverride;
	}

}
