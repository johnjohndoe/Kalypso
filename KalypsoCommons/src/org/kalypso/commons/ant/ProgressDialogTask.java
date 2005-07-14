/*--------------- Kalypso-Header ------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 --------------------------------------------------------------------------*/

package org.kalypso.commons.ant;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;

/**
 * A helper class for ant task which want to run in the kalypso environment.
 * <p>
 * In kalpso we want to have error handling and progress monitors, which is not so easy to have within ant tasks
 * </p>.
 * <p>
 * For this purpose, the real business is delegated done in the
 * {@link org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(IProgressMonitor)}method of
 * this class, which must be implemented by its implementors.
 * </p>
 * <p>
 * Everything done in this method is shown in a nice progress monitor
 * </p>.
 * <p>
 * To have nice error handling, overwrite
 * 
 * @author belger
 */
public abstract class ProgressDialogTask extends Task implements ICoreRunnableWithProgress
{
  /**
   * Imlpementaor shall not overwrite this method. Implement {@link #validateInput()}to do everything which must be
   * done before we execute.
   * <p>
   * Implement {@link ICoreRunnableWithProgress#execute(IProgressMonitor)}for your business
   * </p>.
   * 
   * @see org.apache.tools.ant.Task#execute()
   */
  public final void execute() throws BuildException
  {
    // give implementing class a chance to validate its input
    validateInput();

    // we retrieve the active shel, wathever it is
    final Display display = PlatformUI.getWorkbench().getDisplay();
    // this must be done in the UI-Thread...
    final Runnable runnable = new Runnable()
    {
      public void run()
      {
        final Shell shell = display.getActiveShell();

        // run the execute method in a Progress-Dialog
        final ProgressMonitorDialog dialog = new ProgressMonitorDialog( shell );
        final IStatus status = RunnableContextHelper.execute( dialog, true, true, ProgressDialogTask.this );
        handleError( shell, status );
      }
    };
    display.asyncExec( runnable );
  }

  /**
   * Called before {@link ICoreRunnableWithProgress#execute(IProgressMonitor)}is called.
   * <p>
   * Should be used to validate the input to this task.
   * </p>
   */
  protected abstract void validateInput();

  /**
   * Called after {@link ICoreRunnableWithProgress#execute(IProgressMonitor)}was called, with its return status.
   * <p>
   * Should be used to display an error message to the user
   * <p>
   * 
   * Typical usage is:
   * 
   * <pre>
   *  ErrorDialog.openError( shell, "Dialog Title", "Dialog Message", status );
   * </pre>
   */
  protected abstract void handleError( final Shell shell, final IStatus status );
}
