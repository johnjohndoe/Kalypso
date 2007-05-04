package org.kalypso.kalypso1d2d.pjt.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.ISources;

import de.renew.workflow.connector.WorkflowConnector;

public class NewProjectHandler extends AbstractHandler
{

  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IEvaluationContext applicationContext = (IEvaluationContext) event.getApplicationContext();
    final ISelection selection = (ISelection) applicationContext.getVariable( ISources.ACTIVE_CURRENT_SELECTION_NAME );
    if( !selection.isEmpty() && selection instanceof IStructuredSelection )
    {
      final IStructuredSelection structuredSelection = (IStructuredSelection) selection;
      final Object firstElement = structuredSelection.getFirstElement();
      if( firstElement instanceof IProject )
      {
        return ((IProject) firstElement).getName();
      }
    }
    throw new ExecutionException( "No valid selected project" );
  }

  /**
   * @see org.eclipse.core.commands.AbstractHandler#isEnabled()
   */
  @Override
  public boolean isEnabled( )
  {
    return WorkflowConnector.getConnector().isWorkflowMode();
  }

}
