/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
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
  public Object execute( final ExecutionEvent event )
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );
    final ISelection selection = (ISelection) context.getVariable( ISources.ACTIVE_CURRENT_SELECTION_NAME );
    createWorkflowData( shell, canHandle( selection ) );
    return Status.OK_STATUS;
  }

  private Scenario canHandle( final ISelection selection )
  {
    if( selection instanceof IStructuredSelection )
    {
      IStructuredSelection structSel = ((IStructuredSelection) selection);

      if( !structSel.isEmpty() )
      {
        final Object object = structSel.getFirstElement();
        if( object instanceof Scenario )
          return (Scenario) object;
        else
          return null;
      }
    }
    return null;
  }

  private void createWorkflowData( final Shell shell, final Scenario scenario )
  {
    NewSimulationModelControlBuilder.startWizard( shell, scenario );
  }

}
