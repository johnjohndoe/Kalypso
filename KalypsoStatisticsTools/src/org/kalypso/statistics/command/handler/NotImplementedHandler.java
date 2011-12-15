package org.kalypso.statistics.command.handler;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.kalypso.statistics.utils.AppUtils;

public class NotImplementedHandler extends AbstractHandler implements IHandler {
	
	public static final String ID = NotImplementedHandler.class.getCanonicalName();

	@Override
	public Object execute(ExecutionEvent arg0) throws ExecutionException {
		Display.getDefault().asyncExec(new Runnable() {
			@Override
			public void run() {
				MessageDialog.openInformation(Display.getDefault().getActiveShell(), AppUtils.APPLICATION_TITLE, "This function is not implemented yet.");
			}
		});
		return Status.OK_STATUS;
	}

}
