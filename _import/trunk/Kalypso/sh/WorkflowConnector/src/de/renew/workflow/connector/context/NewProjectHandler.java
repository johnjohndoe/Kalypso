package de.renew.workflow.connector.context;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.ISources;

import de.renew.workflow.cases.CaseData;
import de.renew.workflow.connector.worklist.AbstractWorkflowHandler;

public class NewProjectHandler extends AbstractWorkflowHandler
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
        final String name = ((IProject) firstElement).getName();
        final Object parameter = getState( "activityParameter" ).getValue();
        if( parameter instanceof CaseData )
        {
          final CaseData caseData = (CaseData) parameter;
          caseData.setData( "name", name );
          return name;
        }
      }
    }
    return null;
  }
}
