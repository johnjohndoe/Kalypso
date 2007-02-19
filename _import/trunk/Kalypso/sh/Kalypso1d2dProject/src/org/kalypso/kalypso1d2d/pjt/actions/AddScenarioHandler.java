/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt.actions;

import java.util.logging.Level;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.kalypso.afgui.model.IWorkflowData;
import org.kalypso.kalypso1d2d.pjt.ActiveWorkContext;
import org.kalypso.kalypso1d2d.pjt.wizards.NewSimulationModelControlBuilder;

import de.renew.workflow.WorkflowCommandHandler;

/**
 * @author Patrice Congo, Stefan Kurzbach
 */
public class AddScenarioHandler extends WorkflowCommandHandler
{

  protected ActiveWorkContext activeWorkContext = ActiveWorkContext.getInstance();

  /**
   * @see de.renew.workflow.WorkflowCommandHandler#executeInternal(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public IStatus executeInternal( final ExecutionEvent event )
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );
    final ISelection selection = (ISelection) context.getVariable( ISources.ACTIVE_CURRENT_SELECTION_NAME );
    final Object canHandle = canHandle( selection );
    if( canHandle != null )
    {
      createWorkflowData( shell, canHandle );
      return Status.OK_STATUS;
    }
    else
      return Status.CANCEL_STATUS;
  }

  private Object canHandle( ISelection selection )
  {
    logger.info( "canHandleSlection:" + selection );
    if( selection instanceof IStructuredSelection )
    {
      IStructuredSelection structSel = ((IStructuredSelection) selection);

      if( structSel.size() > 1 )
      {
        return activeWorkContext;
      }

      Object object = structSel.getFirstElement();
      // IWorkflowData parent=null;
      if( object instanceof IWorkflowData )
      {
        // parent=(IWorkflowData)object;

      }
      else
      {
        logger.warning( "No Workflow data selcted:" + object );
        object = activeWorkContext;
      }
      return object;

    }
    else
    {
      logger.warning( "Cannot handle selection:" + selection );
      return activeWorkContext;
    }
  }

  private void createWorkflowData( final Shell shell, final Object dataContext )
  {
    logger.info( "Creating Data:" + dataContext );
    IWorkflowData workflowData = (dataContext instanceof IWorkflowData) ? (IWorkflowData) dataContext : null;
    try
    {
      NewSimulationModelControlBuilder.startWizard( shell, workflowData );
    }
    catch( Throwable th )
    {
      logger.log( Level.SEVERE, "Error creating data", th );
    }
  }

}
