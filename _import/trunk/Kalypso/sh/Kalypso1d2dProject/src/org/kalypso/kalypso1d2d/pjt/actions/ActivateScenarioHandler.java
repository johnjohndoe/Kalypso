package org.kalypso.kalypso1d2d.pjt.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.ISources;
import org.kalypso.afgui.scenarios.Scenario;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;

public class ActivateScenarioHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event )
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final ISelection selection = (ISelection) context.getVariable( ISources.ACTIVE_CURRENT_SELECTION_NAME );
    if( !selection.isEmpty() && selection instanceof IStructuredSelection )
    {
      final IStructuredSelection structuredSelection = (IStructuredSelection) selection;
      final Object firstElement = structuredSelection.getFirstElement();
      if( firstElement instanceof Scenario )
      {
        final Scenario scenario = (Scenario) firstElement;
        Kalypso1d2dProjectPlugin.getDefault().getActiveWorkContext().setCurrentSzenario( scenario );
      }
    }
    return Status.OK_STATUS;
  }

}
