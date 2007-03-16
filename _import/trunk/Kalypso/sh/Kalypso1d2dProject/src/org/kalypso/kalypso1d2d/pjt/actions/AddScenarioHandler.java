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
import org.kalypso.afgui.scenarios.Scenario;
import org.kalypso.kalypso1d2d.pjt.wizards.NewSimulationModelControlBuilder;

import de.renew.workflow.WorkflowCommandHandler;

/**
 * @author Patrice Congo, Stefan Kurzbach
 */
public class AddScenarioHandler extends WorkflowCommandHandler
{
  /**
   * @see de.renew.workflow.WorkflowCommandHandler#executeInternal(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public IStatus executeInternal( final ExecutionEvent event )
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
    else
    {
      logger.warning( "Cannot handle selection:" + selection );
    }
    return null;
  }

  private void createWorkflowData( final Shell shell, final Scenario scenario )
  {
    logger.info( "Creating Data: " + scenario.getURI() );
    try
    {
      NewSimulationModelControlBuilder.startWizard( shell, scenario );
    }
    catch( Throwable th )
    {
      logger.log( Level.SEVERE, "Error creating data", th );
    }
  }

}
