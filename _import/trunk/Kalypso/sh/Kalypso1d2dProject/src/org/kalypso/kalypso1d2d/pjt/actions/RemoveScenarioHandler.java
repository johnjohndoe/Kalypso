/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt.actions;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import de.renew.workflow.WorkflowCommandHandler;

/**
 * @author pat_dev, Stefan Kurzbach
 *
 */
public class RemoveScenarioHandler extends WorkflowCommandHandler
{		
  
  /**
   * @see de.renew.workflow.WorkflowCommandHandler#executeInternal(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  protected IStatus executeInternal( final ExecutionEvent event )
  {
    // final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    // final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );
    // final ISelection selection = (ISelection) context.getVariable( ISources.ACTIVE_CURRENT_SELECTION_NAME );
    
    return Status.CANCEL_STATUS;
  }
}
