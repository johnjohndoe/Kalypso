package org.kalypso.ui.action;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.kalypso.eclipse.core.runtime.jobs.MutexSchedulingRule;
import org.kalypso.ui.nature.ModelNature;

/**
 * @author belger
 */
public class UpdateCalcCaseTimeseries implements IWorkbenchWindowActionDelegate
{
  private IWorkbenchWindow m_window;

  /**
   * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#dispose()
   */
  public void dispose()
  {
    // nix tun  
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
    final ISelection selection = m_window.getSelectionService().getSelection( IPageLayout.ID_RES_NAV );

    // Rechenfälle raussuchen
    final IFolder[] calcCases = CalcCaseHelper.chooseCalcCases( m_window.getShell(), selection, "Zeitreihen aktualisieren", "Folgende Rechenvarianten werden aktualisiert:" );
    
    if( calcCases == null )
      return;
    
    // die Rechenfälle sollen nacheinander aktualisiert werden
    // parallelität macht hier keinen Sinn
    final MutexSchedulingRule mutexRule = new MutexSchedulingRule(  );
    
    // alle Rechenfälle aktualisieren
    for( int i = 0; i < calcCases.length; i++ )
    {
      final IFolder calcCase = calcCases[i];

      final Job job = new Job( "Aktualisiere Zeitreihen: " + calcCase.getName() )
      {
        /**
         * @see org.eclipse.core.internal.jobs.InternalJob#run(org.eclipse.core.runtime.IProgressMonitor)
         */
        protected IStatus run( final IProgressMonitor monitor )
        {
          try
          {
            final ModelNature nature = (ModelNature)calcCase.getProject().getNature( ModelNature.ID );
            
            nature.updateCalcCase( calcCase, monitor );
          }
          catch( final CoreException e )
          {
            e.printStackTrace();

            return e.getStatus();
          }

          return Status.OK_STATUS;
        }
      };
      job.setUser( true );
      job.setRule( mutexRule );
      job.schedule();
    }
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    // mir wurscht
  }
}
