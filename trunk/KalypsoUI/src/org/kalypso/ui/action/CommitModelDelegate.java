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
import org.kalypso.services.user.UserServiceConstants;
import org.kalypso.ui.KalypsoGisPlugin;
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
          && !KalypsoGisPlugin.getDefault().checkUserRight( UserServiceConstants.RIGHT_ADMIN ) )
      {
        MessageDialog
            .openWarning(
                m_window.getShell(),
                "Modell zurückspeichern",
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
      ErrorDialog.openError( m_window.getShell(), "Modell zurückspeichern", "Modell konnte nicht zurückgespeichert werden", ce.getStatus() );
    }
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
  // wurscht  
  }

}
