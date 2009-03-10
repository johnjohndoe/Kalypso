/**
 *
 */
package org.kalypso.afgui.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.kalypso.afgui.scenarios.IScenario;

/**
 * @author Patrice Congo, Stefan Kurzbach
 */
public class AddScenarioHandler extends AbstractHandler
{
  /**
   * @see de.renew.workflow.WorkflowCommandHandler#executeInternal(org.eclipse.core.commands.ExecutionEvent)
   */
  public Object execute( final ExecutionEvent event )
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );
    final ISelection selection = (ISelection) context.getVariable( ISources.ACTIVE_CURRENT_SELECTION_NAME );
    if( selection instanceof IStructuredSelection )
    {
      final IStructuredSelection structSel = ((IStructuredSelection) selection);

      if( !structSel.isEmpty() )
      {
        final Object o = structSel.getFirstElement();
        if( o instanceof IScenario )
        {
          final IScenario scenario = (IScenario) o;
          final IProject project = scenario.getProject();
          NewSimulationModelControlBuilder.startWizard( shell, scenario, project );
        }
        else if( o instanceof IResource )
        {
          final IResource res = (IResource) o;
          final IProject project = res.getProject();
          NewSimulationModelControlBuilder.startWizard( shell, null, project );
        }
      }
    }
    return Status.OK_STATUS;
  }
}
