package org.kalypso.ui.action;

import java.util.Iterator;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.dialogs.ListSelectionDialog;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.kalypso.ui.nature.CalcCaseCollector;
import org.kalypso.ui.nature.ModelNature;

/**
 * @author belger
 */
public class StartCalculationActionDelegate implements IWorkbenchWindowActionDelegate
{
  private IWorkbenchWindow m_window;

  /**
   * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#dispose()
   */
  public void dispose()
  {
  // nix zu tun?
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

    // rausfinden, ob selection ok ist
    if( !( selection instanceof IStructuredSelection ) )
      return;

    final CalcCaseCollector visitor = new CalcCaseCollector();
    try
    {
      final IStructuredSelection structsel = (IStructuredSelection)selection;
      for( final Iterator sIt = structsel.iterator(); sIt.hasNext(); )
      {
        final Object sel = sIt.next();
        if( sel instanceof IContainer )
          ( (IContainer)sel ).accept( visitor );
      }
    }
    catch( final CoreException e )
    {
      e.printStackTrace();

      ErrorDialog.openError( m_window.getShell(), "Berechnung starten",
          "Fehler beim Ermitteln der Rechenfälle", e.getStatus() );
    }

    final IFolder[] calcCases = visitor.getCalcCases();
    if( calcCases.length == 0 )
    {
      MessageDialog.openInformation( m_window.getShell(), "Berechnung starten",
          "Es sind keine Rechenfälle selektiert." );
      return;
    }

    final ListSelectionDialog dlg = new ListSelectionDialog( m_window.getShell(), calcCases,
        new ArrayContentProvider(), new WorkbenchLabelProvider(),
        "Welche Rechenvarianten sollen berechnet werden?" );
    dlg.setInitialSelections( calcCases );
    if( dlg.open() == Window.CANCEL )
      return;

    final Object[] calcCasesToCalc = dlg.getResult();
    
    for( int i = 0; i < calcCasesToCalc.length; i++ )
    {
      final IFolder folder = (IFolder)calcCasesToCalc[i];

      final Job job = new Job( "Berechne: " + folder.getName() )
      {
        /**
         * @see org.eclipse.core.internal.jobs.InternalJob#run(org.eclipse.core.runtime.IProgressMonitor)
         */
        protected IStatus run( final IProgressMonitor monitor )
        {
          try
          {
            final ModelNature nature = (ModelNature)folder.getProject().getNature( ModelNature.ID );
            nature.runCalculation( folder, monitor );
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
      job.schedule();
    }
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    // mir doch egal!
  }

}