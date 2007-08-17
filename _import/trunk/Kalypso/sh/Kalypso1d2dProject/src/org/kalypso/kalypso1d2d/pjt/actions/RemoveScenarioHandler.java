/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt.actions;

import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.afgui.ScenarioHandlingProjectNature;
import org.kalypso.afgui.scenarios.Scenario;
import org.kalypso.afgui.scenarios.ScenarioList;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;

import de.renew.workflow.connector.cases.ICaseManager;

/**
 * @author pat_dev, Stefan Kurzbach
 */
public class RemoveScenarioHandler extends AbstractHandler
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
    if( !selection.isEmpty() && selection instanceof IStructuredSelection )
    {
      final IStructuredSelection structuredSelection = (IStructuredSelection) selection;
      final Object firstElement = structuredSelection.getFirstElement();
      if( firstElement instanceof Scenario )
      {
        final Scenario scenario = (Scenario) firstElement;
        final ScenarioList derivedScenarios = scenario.getDerivedScenarios();
        if( derivedScenarios != null && !derivedScenarios.getScenarios().isEmpty() )
        {
          MessageDialog.openInformation( shell, Messages.getString("RemoveScenarioHandler.0"), Messages.getString("RemoveScenarioHandler.1") ); //$NON-NLS-1$ //$NON-NLS-2$
          return Status.CANCEL_STATUS;
        }
        else
        {
          final String projectName = ScenarioHelper.getProjectName( scenario );
          final IWorkspace workspace = ResourcesPlugin.getWorkspace();
          final IProject project = workspace.getRoot().getProject( projectName );
          try
          {
            final ScenarioHandlingProjectNature nature = ScenarioHandlingProjectNature.toThisNature( project );
            final ICaseManager<Scenario> scenarioManager = nature.getCaseManager();
            final List<Scenario> rootScenarios = scenarioManager.getCases();
            if( rootScenarios.contains( scenario ) && rootScenarios.size() == 1 )
            {
              MessageDialog.openInformation( shell, Messages.getString("RemoveScenarioHandler.2"), Messages.getString("RemoveScenarioHandler.3") ); //$NON-NLS-1$ //$NON-NLS-2$
              return Status.CANCEL_STATUS;
            }
            else if( Kalypso1d2dProjectPlugin.getDefault().getActiveWorkContext().getCurrentCase() == scenario )
            {
              MessageDialog.openInformation( shell, Messages.getString("RemoveScenarioHandler.4"), Messages.getString("RemoveScenarioHandler.5") ); //$NON-NLS-1$ //$NON-NLS-2$
              return Status.CANCEL_STATUS;
            }
            else if(MessageDialog.openConfirm( shell, "Bitte bestätigen", "Wollen Sie das Szenario wirklich löschen?" ))
            {
              final UIJob runnable = new UIJob( shell.getDisplay(), Messages.getString("RemoveScenarioHandler.6") ) //$NON-NLS-1$
              {
                /**
                 * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
                 */
                @Override
                public IStatus runInUIThread( final IProgressMonitor monitor )
                {
                  try
                  {
                    scenarioManager.removeCase( scenario, monitor );
                  }
                  catch( final CoreException e )
                  {
                    return e.getStatus();
                  }
                  return Status.OK_STATUS;
                }
              };
              runnable.schedule();
            }
          }
          catch( final CoreException e )
          {
            throw new ExecutionException( Messages.getString("RemoveScenarioHandler.7"), e ); //$NON-NLS-1$
          }
        }
      }
    }
    return Status.CANCEL_STATUS;
  }
}
