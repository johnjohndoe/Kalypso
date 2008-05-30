/**
 * 
 */
package org.kalypso.afgui.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.kalypso.afgui.scenarios.Scenario;
import org.kalypso.afgui.scenarios.ScenarioHelper;

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
    if( selection instanceof IStructuredSelection )
    {
      IStructuredSelection structSel = ((IStructuredSelection) selection);

      if( !structSel.isEmpty() )
      {
        final Object o = structSel.getFirstElement();
        final IProject project;
        if( o instanceof Scenario )
        {
          final Scenario scenario = (Scenario) o;
          final String projectName = ScenarioHelper.getProjectName( scenario );
          final IWorkspace workspace = ResourcesPlugin.getWorkspace();
          project = workspace.getRoot().getProject( projectName );
          NewSimulationModelControlBuilder.startWizard( shell, scenario, project );
        }
        else if( o instanceof IResource )
        {
          IResource res = (IResource) o;
          project = res.getProject();
          NewSimulationModelControlBuilder.startWizard( shell, null, project );
        }
      }
    }
    return Status.OK_STATUS;
  }
}
