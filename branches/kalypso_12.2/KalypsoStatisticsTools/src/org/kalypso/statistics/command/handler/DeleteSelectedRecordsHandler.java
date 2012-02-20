package org.kalypso.statistics.command.handler;

import java.util.List;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.kalypso.statistics.db.handler.AbstractRecordDBHandler;
import org.kalypso.statistics.db.handler.DBResponse;
import org.kalypso.statistics.gui.views.IKalypsoStatsRecordViewerSelectable;
import org.kalypso.statistics.types.data.AbstractStatisticsRecordType;
import org.kalypso.statistics.utils.AppUtils;

public class DeleteSelectedRecordsHandler extends AbstractSelectionListRecordsHandler {

	public static final String ID = DeleteSelectedRecordsHandler.class.getCanonicalName();

	@Override
	protected boolean viewerPreProcessAction(final IKalypsoStatsRecordViewerSelectable<? extends AbstractStatisticsRecordType> view,
			final List<? extends AbstractStatisticsRecordType> selection) {
		final String canDeleteRecordsMessage = view.cannotDeleteRecordsErrMessage(selection);
		if (canDeleteRecordsMessage == null) {
			final String[] message = view.getCustomMessage(IKalypsoStatsRecordViewerSelectable.TYPE.DELETE);
			return MessageDialog.openQuestion(Display.getDefault().getActiveShell(), message[0], message[1]);
		} else {
			MessageDialog.openInformation(Display.getDefault().getActiveShell(), AppUtils.APPLICATION_TITLE, canDeleteRecordsMessage);
			return false;
		}
	}

	@Override
	protected DBResponse executeCommand(final AbstractRecordDBHandler<? extends AbstractStatisticsRecordType> dbHandler, final List<? extends AbstractStatisticsRecordType> records) {
		for (final AbstractStatisticsRecordType record : records) {
			final DBResponse dbResponse = dbHandler.deleteRecord(record);
			if (!dbResponse.isOK())
				return dbResponse;
		}
		return DBResponse.OK();
	}

	@Override
	protected String getID() {
		return ID;
	}

}
