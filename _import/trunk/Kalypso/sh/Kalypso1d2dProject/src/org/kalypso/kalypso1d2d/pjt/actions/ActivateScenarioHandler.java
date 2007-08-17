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
import org.kalypso.kalypso1d2d.pjt.views.TaskExecutionAuthority;

import de.renew.workflow.connector.context.ActiveWorkContext;
import de.renew.workflow.connector.worklist.ITaskExecutor;

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
        final ActiveWorkContext<Scenario> activeWorkContext = plugin.getActiveWorkContext();
        try
        {
          final ITaskExecutor taskExecutor = plugin.getTaskExecutor();
          final TaskExecutionAuthority taskExecutionAuthority = plugin.getTaskExecutionAuthority();
          if( activeWorkContext.getCurrentCase() != scenario && taskExecutionAuthority.canStopTask( taskExecutor.getActiveTask() ) )
          {
            taskExecutor.stopActiveTask();
            activeWorkContext.setCurrentCase( scenario );
          }
        }
        catch( final CoreException e )
        {
          final Shell shell = HandlerUtil.getActiveShellChecked( event );
          final IStatus status = e.getStatus();
          ErrorDialog.openError( shell, Messages.getString("ActivateScenarioHandler.0"), Messages.getString("ActivateScenarioHandler.1"), status ); //$NON-NLS-1$ //$NON-NLS-2$
          plugin.getLog().log( status );
        }
      }
    }
    return Status.OK_STATUS;
  }

}
