/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.kalypso.afgui.scenarios.Scenario;
import org.kalypso.kalypso1d2d.pjt.wizards.NewSimulationModelControlBuilder;

/**
 * @author Patrice Congo, Stefan Kurzbach
 */
public class AddScenarioHandler extends AbstractHandler
{
  /**
   * @see de.renew.workflow.WorkflowCommandHandler#executeInternal(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );
    final ISelection selection = (ISelection) context.getVariable( ISources.ACTIVE_CURRENT_SELECTION_NAME );
    final Scenario canHandle = canHandle( selection );
    if( canHandle != null )
    {
      createWorkflowData( shell, canHandle );
      return Status.OK_STATUS;
    }
    else
      return Status.CANCEL_STATUS;
  }

  private Scenario canHandle( final ISelection selection )
  {
    if( selection instanceof IStructuredSelection )
    {
      IStructuredSelection structSel = ((IStructuredSelection) selection);

      if( !structSel.isEmpty() )
      {
        final Object object = structSel.getFirstElement();
        return (Scenario) object;
      }
    }
    return null;
  }

  private void createWorkflowData( final Shell shell, final Scenario scenario )
  {
    NewSimulationModelControlBuilder.startWizard( shell, scenario );
  }

}
