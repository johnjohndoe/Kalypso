package org.kalypso.ui.action;

import java.io.File;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
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
    final ISelection selection = m_window.getSelectionService().getSelection(
        IPageLayout.ID_RES_NAV );

    final IProject[] projects = ResourceUtilities.findeProjectsFromSelection( selection );

    if( projects == null || projects.length != 1 )
    {
      MessageDialog.openInformation( m_window.getShell(), "Modell zurückspeichern",
          "Bitte wählen Sie genau ein Projekt im Navigator aus" );
      return;
    }

    final IProject project = projects[0];

    
    
    final String name = project.getName();
    final File serverRoot = KalypsoGisPlugin.getDefault().getServerModelRoot();
    final File serverProject = new File( serverRoot, name );
    
    // nur Administratoren dürfen Projekte überschreiben
    if( serverProject.exists() && !KalypsoGisPlugin.getDefault().checkUserRight( UserServiceConstants.RIGHT_ADMIN ) )
    {
      MessageDialog.openWarning( m_window.getShell(), "Modell zurückspeichern", "Das Modell existiert bereits auf dem Server.\nNur Administratoren dürfen bereits vorhandene Modelle überschreiben." );
      return;
    }

    if( !MessageDialog.openConfirm( m_window.getShell(), "Modell zurückspeichern", "Das Projekt '" + name + "' wird zurück auf den Server gespeichert.\nSind Sie sicher? Serverseitige Änderungen gehen eventuell verloren." ) )
      return;
    
    // TODO implement it
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
    job.setUser(true);
    job.schedule();
    
    return;
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
  // wurscht  
  }

}
