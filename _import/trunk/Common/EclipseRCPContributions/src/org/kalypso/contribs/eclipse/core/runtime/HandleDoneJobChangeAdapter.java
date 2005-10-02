package org.kalypso.contribs.eclipse.core.runtime;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Shell;

/**
 * Waits for {@link org.eclipse.core.runtime.jobs.Job}to finish and then shows a message box if the job was succesful.
 * 
 * @see IStatus#isOK()
 * 
 * @author belger
 */
public final class HandleDoneJobChangeAdapter extends AutoRemoveJobChangeAdapter
{
  private final Shell m_shell;

  private final String m_messageTitle;

  private final String m_messageFirstline;

  private final int m_stateMask;

  /**
   * Same as {@link HandleDoneJobChangeAdapter#HandleDoneJobChangeAdapter(Shell, String, String, boolean, int)},
   * stateMask defaulting to <code>IStatus.CANCEL | IStatus.INFO | IStatus.WARNING</code>.
   */
  public HandleDoneJobChangeAdapter( final Shell shell, final String messageTitle, final String messageFirstline,
      final boolean autoRemoveListener )
  {
    this( shell, messageTitle, messageFirstline, autoRemoveListener, IStatus.CANCEL | IStatus.INFO | IStatus.WARNING );
  }

  /**
   * @param shell
   *          Shell to show MessageBox with. Must not be null.
   * @param messageTitle
   *          Displayed in the title of the message box.
   * @param messageFirstline
   *          Displayed as first line of the message box. The second line wil be the mesage of the status objekt.
   * @param stateMask
   *          the stateMask for which to display an error-dialog.
   * @see ErrorDialog#openError(org.eclipse.swt.widgets.Shell, java.lang.String, java.lang.String,
   *      org.eclipse.core.runtime.IStatus, int)
   */
  public HandleDoneJobChangeAdapter( final Shell shell, final String messageTitle, final String messageFirstline,
      final boolean autoRemoveListener, final int stateMask )
  {
    super( autoRemoveListener );

    m_shell = shell;
    m_messageTitle = messageTitle;
    m_messageFirstline = messageFirstline;
    m_stateMask = stateMask;
  }

  /**
   * @see org.eclipse.core.runtime.jobs.JobChangeAdapter#done(org.eclipse.core.runtime.jobs.IJobChangeEvent)
   */
  public void done( final IJobChangeEvent event )
  {
    final int stateMask = m_stateMask;
    final Shell shell = m_shell;
    final String messageTitle = m_messageTitle;
    final String messageFirstline = m_messageFirstline;

    final Runnable runnable = new Runnable()
    {
      public void run()
      {
        final IStatus status = event.getResult();
        final ErrorDialog dialog = new ErrorDialog( shell, messageTitle, messageFirstline, status, stateMask );
        dialog.open();
      }
    };
    m_shell.getDisplay().asyncExec( runnable );

    super.done( event );
  }
}