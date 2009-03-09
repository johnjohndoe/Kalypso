/**
 *
 */
package org.kalypso.afgui.handlers;

import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IProject;
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
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.ScenarioHandlingProjectNature;
import org.kalypso.afgui.scenarios.IScenario;
import org.kalypso.afgui.scenarios.IScenarioList;
import org.kalypso.afgui.i18n.Messages;
import de.renew.workflow.connector.cases.ICaseManager;

/**
 * @author Stefan Kurzbach
 */
public class RemoveScenarioHandler extends AbstractHandler
{

  /**
   * @see de.renew.workflow.WorkflowCommandHandler#executeInternal(org.eclipse.core.commands.ExecutionEvent)
   */
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );
    final ISelection selection = (ISelection) context.getVariable( ISources.ACTIVE_CURRENT_SELECTION_NAME );
    if( !selection.isEmpty() && selection instanceof IStructuredSelection )
    {
      final IStructuredSelection structuredSelection = (IStructuredSelection) selection;
      final Object firstElement = structuredSelection.getFirstElement();
      if( firstElement instanceof IScenario )
      {
        final IScenario scenario = (IScenario) firstElement;
        final IScenarioList derivedScenarios = scenario.getDerivedScenarios();
        if( derivedScenarios != null && !derivedScenarios.getScenarios().isEmpty() )
        {
          MessageDialog.openInformation( shell, Messages.getString( "org.kalypso.afgui.handlers.RemoveScenarioHandler.0" ), Messages.getString( "org.kalypso.afgui.handlers.RemoveScenarioHandler.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
          return Status.CANCEL_STATUS;
        }
        else
        {
          final IProject project = scenario.getProject();
          try
          {
            final ScenarioHandlingProjectNature nature = ScenarioHandlingProjectNature.toThisNature( project );
            final ICaseManager<IScenario> scenarioManager = nature.getCaseManager();
            final List<IScenario> rootScenarios = scenarioManager.getCases();
            if( rootScenarios.contains( scenario ) && rootScenarios.size() == 1 )
            {
              MessageDialog.openInformation( shell, Messages.getString( "org.kalypso.afgui.handlers.RemoveScenarioHandler.2" ), Messages.getString( "org.kalypso.afgui.handlers.RemoveScenarioHandler.3" ) ); //$NON-NLS-1$ //$NON-NLS-2$
              return Status.CANCEL_STATUS;
            }
            else if( KalypsoAFGUIFrameworkPlugin.getDefault().getActiveWorkContext().getCurrentCase() == scenario )
            {
              MessageDialog.openInformation( shell, Messages.getString( "org.kalypso.afgui.handlers.RemoveScenarioHandler.4" ), Messages.getString( "org.kalypso.afgui.handlers.RemoveScenarioHandler.5" ) ); //$NON-NLS-1$ //$NON-NLS-2$
              return Status.CANCEL_STATUS;
            }
            else if( MessageDialog.openConfirm( shell, Messages.getString("org.kalypso.afgui.handlers.RemoveScenarioHandler.7"), Messages.getString("org.kalypso.afgui.handlers.RemoveScenarioHandler.8") ) ) //$NON-NLS-1$ //$NON-NLS-2$
            {
              final UIJob runnable = new UIJob( shell.getDisplay(), Messages.getString( "org.kalypso.afgui.handlers.RemoveScenarioHandler.6" ) ) //$NON-NLS-1$
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
            throw new ExecutionException( "", e ); //$NON-NLS-1$
          }
        }
      }
    }
    return Status.CANCEL_STATUS;
  }
}
