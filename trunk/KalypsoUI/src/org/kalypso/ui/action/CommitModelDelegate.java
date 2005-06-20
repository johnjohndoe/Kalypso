/*--------------- Kalypso-Header --------------------------------------------------------------------

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
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.action;

import java.io.File;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.users.UserServiceConstants;
import org.kalypso.util.synchronize.ModelSynchronizer;

/**
 * @author bce
 */
public class CommitModelDelegate implements IWorkbenchWindowActionDelegate
{
  private IWorkbenchWindow m_window;

  /**
   * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#dispose()
   */
  public void dispose()
  {
  // nichts tun
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#init(org.eclipse.ui.IWorkbenchWindow)
   */
  public void init( final IWorkbenchWindow window )
  {
    m_window = window;
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( IAction action )
  {
    try
    {
      final IProject project = ModelActionHelper.chooseOneProject( m_window );
      final File serverRoot = ModelActionHelper.getServerRoot();

      final String name = project.getName();
      final File serverProject = new File( serverRoot, name );

      // nur Administratoren dürfen Projekte überschreiben
      if( serverProject.exists()
          && !KalypsoGisPlugin.getDefault().getUser().hasRight( UserServiceConstants.RIGHT_ADMIN ) )
      {
        MessageDialog
            .openWarning( m_window.getShell(), "Modell zurückspeichern",
                "Das Modell existiert bereits auf dem Server.\nNur Administratoren dürfen bereits vorhandene Modelle überschreiben." );
        return;
      }

      if( !MessageDialog
          .openConfirm(
              m_window.getShell(),
              "Modell zurückspeichern",
              "Das Projekt '"
                  + name
                  + "' wird zurück auf den Server gespeichert.\nSind Sie sicher? Serverseitige Änderungen gehen eventuell verloren." ) )
        return;

      final ModelSynchronizer synchronizer = new ModelSynchronizer( project, serverProject );

      final Job job = new Job( "Modell zurückspeichern" )
      {

        protected IStatus run( IProgressMonitor monitor )
        {
          try
          {
            synchronizer.commitProject();
          }
          catch( CoreException e )
          {
            return e.getStatus();
          }

          return Status.OK_STATUS;
        }
      };
      job.setUser( true );
      job.schedule();
    }
    catch( final CoreException ce )
    {
      ErrorDialog.openError( m_window.getShell(), "Modell zurückspeichern",
          "Modell konnte nicht zurückgespeichert werden", ce.getStatus() );
    }
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
  // wurscht
  }

}
