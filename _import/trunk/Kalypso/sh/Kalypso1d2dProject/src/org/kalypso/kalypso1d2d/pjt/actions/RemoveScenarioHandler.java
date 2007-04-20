/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.runtime.Status;

/**
 * @author pat_dev, Stefan Kurzbach
 */
public class RemoveScenarioHandler extends AbstractHandler
{

  /**
   * @see de.renew.workflow.WorkflowCommandHandler#executeInternal(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( final ExecutionEvent event )
  {
    // final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    // final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );
    // final ISelection selection = (ISelection) context.getVariable( ISources.ACTIVE_CURRENT_SELECTION_NAME );

    return Status.CANCEL_STATUS;
  }
}
