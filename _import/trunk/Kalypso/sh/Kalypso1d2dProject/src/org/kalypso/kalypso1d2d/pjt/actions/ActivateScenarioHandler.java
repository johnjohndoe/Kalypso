package org.kalypso.kalypso1d2d.pjt.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;
import org.kalypso.afgui.scenarios.Scenario;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;

import de.renew.workflow.base.Task;

public class ActivateScenarioHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final ISelection selection = HandlerUtil.getCurrentSelection( event );
    if( selection != null && !selection.isEmpty() && selection instanceof IStructuredSelection )
    {
      final IStructuredSelection structuredSelection = (IStructuredSelection) selection;
      final Object firstElement = structuredSelection.getFirstElement();
      if( firstElement instanceof Scenario )
      {
        final Scenario scenario = (Scenario) firstElement;
        final Kalypso1d2dProjectPlugin plugin = Kalypso1d2dProjectPlugin.getDefault();
        try
        {
          final Task activeTask = plugin.getTaskExecutor().getActiveTask();
          if( activeTask != null && plugin.getTaskExecutionAuthority().canStopTask( activeTask ) )
            plugin.getActiveWorkContext().setCurrentCase( scenario );
        }
        catch( final CoreException e )
        {
          final Shell shell = HandlerUtil.getActiveShellChecked( event );
          final IStatus status = e.getStatus();
          ErrorDialog.openError( shell, "Problem", "Szenario konnte nicht aktiviert werden", status );
          plugin.getLog().log( status );
        }
      }
    }
    return Status.OK_STATUS;
  }

}
