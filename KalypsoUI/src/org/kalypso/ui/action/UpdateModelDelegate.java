package org.kalypso.ui.action;

import java.io.File;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.synchronize.ModelSynchronizer;

/**
 * @author bce
 */
public class UpdateModelDelegate implements IWorkbenchWindowActionDelegate
{
  private IWorkbenchWindow m_window;

  /**
   * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#dispose()
   */
  public void dispose()
  {
  // nichts zu tun
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
  public void run( final IAction action )
  {
    final ISelection selection = m_window.getSelectionService().getSelection(
        IPageLayout.ID_RES_NAV );

    final IProject[] projects = ResourceUtilities.findeProjectsFromSelection( selection );

    if( projects == null || projects.length != 1 )
    {
      MessageDialog.openInformation( m_window.getShell(), "Modell aktualisieren",
          "Bitte wählen Sie genau ein Projekt im Navigator aus" );
      return;
    }

    final IProject project = projects[0];

    final String name = project.getName();
    final File serverRoot = KalypsoGisPlugin.getDefault().getServerModelRoot();
    final File serverProject = new File( serverRoot, name );
    if( !serverProject.exists() )
    {
      MessageDialog.openWarning( m_window.getShell(), "Modell aktualisieren", "Sie haben kein Server-gespeichertes Projekt gewählt.\nNur Server-gespeicherte Projekt können aktualisiert werden." );
      return;
    }
    
    if( !MessageDialog.openConfirm( m_window.getShell(), "Modell aktualisieren",
        "Soll das Projekt '" + project.getName()
            + "' aktualisiert werden?\nLokale Änderungen gehen dadurch verloren." ) )
      return;

    final Job job = new Job( "Modelle aktualisieren" )
    {
      protected IStatus run( final IProgressMonitor monitor )
      {
        monitor.beginTask( "Modelle vom Server laden", 1000 );

        if( monitor.isCanceled() )
          return Status.CANCEL_STATUS;

        try
        {

          final ModelSynchronizer synchronizer = new ModelSynchronizer( project, serverProject );

          synchronizer.updateLocal( new SubProgressMonitor( monitor, 1000 ) );
        }
        catch( final CoreException e )
        {
          e.printStackTrace();

          return e.getStatus();
        }
        finally
        {
          monitor.done();
        }

        return Status.OK_STATUS;
      }
    };
    job.setUser( true );
    job.schedule();
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
  // ignorieren
  }

}