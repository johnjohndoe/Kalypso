package org.kalypso.statistics.command.handler;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.IWorkbenchPart;
import org.kalypso.statistics.gui.PartManager;
import org.kalypso.statistics.gui.views.IKalypsoStatsRecordViewerSelectable;

public class SelectNoneHandler extends AbstractHandler implements IHandler {
	
	public static final String ID = SelectNoneHandler.class.getCanonicalName();

	@Override
	public Object execute(final ExecutionEvent event) throws ExecutionException {
		final IWorkbenchPart part = PartManager.getInstance().getActivePart();
		if (part != null && part instanceof IKalypsoStatsRecordViewerSelectable<?>) {
			final IKalypsoStatsRecordViewerSelectable<?> view = (IKalypsoStatsRecordViewerSelectable<?>) part;
			view.deselectAll();
		}
		return Status.OK_STATUS;
	}

}
