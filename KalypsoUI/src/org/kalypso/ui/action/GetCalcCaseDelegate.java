package org.kalypso.ui.action;

import java.io.File;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.dialogs.ListSelectionDialog;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.synchronize.ModelSynchronizer;

/**
 * @author belger
 */
public class GetCalcCaseDelegate implements IWorkbenchWindowActionDelegate
{
  private IWorkbenchWindow m_window;

  /**
   * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#dispose()
   */
  public void dispose()
  {
  //  
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
    try
    {
      final File serverRoot = ModelActionHelper.getServerRoot();

      final IProject project = ModelActionHelper.chooseOneProject( m_window );

      final File serverProject = ModelActionHelper.checkIsSeverMirrored( serverRoot, project );

      final ModelSynchronizer synchronizer = new ModelSynchronizer( project, serverProject );
      final File[] remoteCalcCases = synchronizer.getRemoteCalcCases();

      final String serverPath = serverProject.getAbsolutePath();
      final int serverPathLength = serverPath.length();
      final String[] remotePathes = new String[remoteCalcCases.length];
      final Map pathHash = new HashMap();
      for( int i = 0; i < remoteCalcCases.length; i++ )
      {
        final File calcDir = remoteCalcCases[i];
        final String calcPath = calcDir.getAbsolutePath();
        if( calcPath.length() > serverPathLength + 1 )
        {
          final String calcPathRel = calcPath.substring( serverPathLength + 1 );
          remotePathes[i] = calcPathRel;
          pathHash.put( calcPathRel, calcDir );
        }
      }

      final ListSelectionDialog dialog = new ListSelectionDialog(
          m_window.getShell(),
          remotePathes,
          new ArrayContentProvider(),
          new LabelProvider(),
          "Wählen Sie die Rechenvarianten, die vom Server geladen werden.\n"
              + "Ist im Arbeitsbereich bereits an gleicher Stelle eine Rechenvariante vorhanden, wird diese überschrieben." );
      if( dialog.open() != Window.OK )
        return;

      final Object[] result = dialog.getResult();

      final Job job = new Job( "Rechenvarianten vom Server laden" )
      {
        protected IStatus run( final IProgressMonitor monitor )
        {
          monitor.beginTask( "Rechenvarianten vom Server laden", 1000 * result.length );

          final Collection errorStati = new LinkedList();

          for( int i = 0; i < result.length; i++ )
          {
            final String relPath = result[i].toString();
            final File calcDir = (File)pathHash.get( relPath );

            try
            {
              synchronizer.getFolder( calcDir, relPath, new SubProgressMonitor( monitor, 1000 ) );
            }
            catch( final CoreException e )
            {
              errorStati.add( e.getStatus() );
              e.printStackTrace();
            }
          }

          if( errorStati.isEmpty() )
            return Status.OK_STATUS;

          final IStatus[] stati = (IStatus[])errorStati.toArray( new IStatus[errorStati.size()] );
          return new MultiStatus( KalypsoGisPlugin.getId(), 0, stati,
              "Nicht alle Rechenvarianten konnten geladen werden.", null );
        }
      };

      job.setUser(true);
      job.schedule();
    }
    catch( final CoreException ce )
    {
      ErrorDialog.openError( m_window.getShell(), "Rechvarianten vom Server laden",
          "Rechenvarianten konnten nicht vom Server geladen werden.", ce.getStatus() );
    }
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
  //
  }

}