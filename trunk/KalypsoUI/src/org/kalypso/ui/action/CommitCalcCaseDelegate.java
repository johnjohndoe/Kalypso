package org.kalypso.ui.action;

import java.io.File;
import java.util.Collection;
import java.util.LinkedList;

import org.eclipse.core.resources.IFolder;
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
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.synchronize.ModelSynchronizer;

/**
 * @author belger
 */
public class CommitCalcCaseDelegate implements IWorkbenchWindowActionDelegate
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
    try
    {
      final File serverRoot = ModelActionHelper.getServerRoot();

      final IProject project = ModelActionHelper.chooseOneProject( m_window );

      final File serverProject = ModelActionHelper.checkIsSeverMirrored( serverRoot, project );

      final ModelSynchronizer synchronizer = new ModelSynchronizer( project, serverProject );

      final ISelection selection = m_window.getSelectionService().getSelection(
          IPageLayout.ID_RES_NAV );
      final IFolder[] calcCases = CalcCaseHelper.chooseCalcCases( m_window.getShell(), selection,
          "Zeitreihen aktualisieren", "Folgende Rechenvarianten werden aktualisiert:" );

      if( calcCases == null )
        return;

      final Job job = new Job( "Rechenvarianten archivieren" )
      {
        protected IStatus run( final IProgressMonitor monitor )
        {
          monitor.beginTask( "Rechenvarianten archivieren", calcCases.length * 1000 );
          
          final Collection errorStati = new LinkedList();
          for( int i = 0; i < calcCases.length; i++ )
          {
            final IFolder folder = calcCases[i];
            
            try
            {
              synchronizer.commitFolder( folder, new SubProgressMonitor( monitor, 1000 ) );
            }
            catch( CoreException e )
            {
              errorStati.add( e.getStatus() );
              e.printStackTrace();
            }
          }

          if( errorStati.isEmpty() )
            return Status.OK_STATUS;

          final IStatus[] stati = (IStatus[])errorStati.toArray( new IStatus[errorStati.size()] );
          return new MultiStatus( KalypsoGisPlugin.getId(), 0, stati,
              "Nicht alle Rechenvarianten konnten archiviert werden.", null );
        }
      };

      job.setUser( true );
      job.schedule();
    }
    catch( final CoreException ce )
    {
      ErrorDialog.openError( m_window.getShell(), "Rechvarianten archivieren",
          "Rechenvarianten können nicht archiviert werden.", ce.getStatus() );
    }
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( final IAction action, final ISelection selection )
  {
  // egal, aktuelle Selektion wird in run ermittelt
  }

}