package org.kalypso.ui.editor.styleeditor.dialogs;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.ui.editor.styleeditor.MessageBundle;

/**
 * @author F.Lindemann
 *  
 */
public class StyleEditorErrorDialog
{

  private IStatus status = null;

  private String message = null;

  private Shell shell = null;

  public StyleEditorErrorDialog( Shell m_shell, String m_message, String reason )
  {
    this.message = m_message;
    status = new Status( IStatus.ERROR, MessageBundle.STYLE_EDITOR_PLUGIN_ID, 0, reason, null );
    this.shell = m_shell;
  }

  public void showError()
  {
    ErrorDialog.openError( shell, MessageBundle.STYLE_EDITOR_ERROR_TITLE, message, status );
  }
}