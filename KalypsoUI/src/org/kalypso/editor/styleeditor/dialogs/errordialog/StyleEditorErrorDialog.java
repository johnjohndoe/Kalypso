/*
 * Created on 09.09.2004
 *
 */
package org.kalypso.editor.styleeditor.dialogs.errordialog;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Shell;

/**
 * @author Administrator
 *
 */
public class StyleEditorErrorDialog {
	
	private IStatus status = null;
	private String message = null;
	private Shell shell = null;
	private final String STYLE_EDITOR_PLUGIN_ID = "org.kalypso.editor.mapeditor.views.styleeditor";
	private final String TITLE ="Error - StyleEditor";
	
	public StyleEditorErrorDialog(Shell shell, String message, String reason)
	{
		this.message = message;
		status = new Status(IStatus.ERROR,STYLE_EDITOR_PLUGIN_ID,0,reason, null);		
		this.shell = shell;
	}
	
	public void showError()
	{
		ErrorDialog.openError(shell, TITLE,message,status);
	}
}
