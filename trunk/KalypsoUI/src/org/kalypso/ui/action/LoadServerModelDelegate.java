package org.kalypso.ui.action;

import java.io.File;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.window.Window;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.dialogs.ListSelectionDialog;
import org.kalypso.eclipse.jface.viewers.FileLabelProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.synchronize.ModelSynchronizer;

/**
 * @author bce
 */
public class LoadServerModelDelegate implements IWorkbenchWindowActionDelegate
{
  private IWorkbenchWindow m_window;

  /**
   * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#dispose()
   */
  public void dispose()
  {
  // nix zu tun
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
    final File serverRoot = KalypsoGisPlugin.getDefault().getServerModelRoot();
    final File[] files = serverRoot.listFiles();

    final ListSelectionDialog lsd = new ListSelectionDialog(
        m_window.getShell(),
        files,
        new ArrayContentProvider(),
        new FileLabelProvider(),
        "Wählen Sie die Modelle, die Sie vom Server laden möchten.\nIm Arbeitsbereich bereits vorhandene Modelle werden aktualisiert." );
    lsd.setInitialSelections( files );
    lsd.setTitle( "Modelle vom Server laden" );
    if( lsd.open() != Window.OK )
      return;
    
    final Object[] projects = lsd.getResult();

    final Job job = new Job( "Modelle aktualisieren" )
    {
      protected IStatus run( final IProgressMonitor monitor )
      {
        monitor.beginTask( "Modelle vom Server laden", files.length * 1000 );

        for( int i = 0; i < projects.length; i++ )
        {
          if( monitor.isCanceled() )
            return Status.CANCEL_STATUS;
          
          final File serverProject = (File)projects[i];
          final String name = serverProject.getName();
          
          final IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject( name );

          final ModelSynchronizer synchronizer = new ModelSynchronizer( project, serverProject );
          
          try
          {
            synchronizer.updateLocal( new SubProgressMonitor( monitor, 1000 ) );
          }
          catch( final CoreException e )
          {
            e.printStackTrace();
            
            return e.getStatus();
          }
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
  // auch wurscht
  }

}