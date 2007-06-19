/**
 * 
 */
package org.kalypso.kalypso1d2d.pjt.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.kalypso1d2d.pjt.views.SzenarioDataProvider;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.CalculationUnitView.CalculationUnitPerformView;

import de.renew.workflow.connector.cases.CaseHandlingSourceProvider;

/**
 * 
 * @author Dejan Antanaskovic
 */
public class CalculationUnitPerformViewHandler extends AbstractHandler implements IExecutableExtension
{
  /**
   * Creates a new {@link FeatureViewInputContextHandler} that loads the given input file
   */
  public CalculationUnitPerformViewHandler( )
  {
  }

  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @SuppressWarnings("unchecked")//$NON-NLS-1$
  @Override
  public Object execute( final ExecutionEvent event )
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final SzenarioDataProvider szenarioDataProvider = (SzenarioDataProvider) context.getVariable( CaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
    try
    {
      final IFEDiscretisationModel1d2d model = szenarioDataProvider.getModel( IFEDiscretisationModel1d2d.class );
      final IWorkbenchWindow window = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
      final IViewPart view = window.getActivePage().findView( CalculationUnitPerformView.ID );
      final CalculationUnitPerformView calculationUnitPerformView = (CalculationUnitPerformView) view;
      calculationUnitPerformView.initialiseModel( model );
    }
    catch( CoreException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

// final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );
// final IFolder scenarioFolder = (IFolder) context.getVariable( CaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
// final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
//
// final IFolder szenarioFolder = (IFolder) context.getVariable( CaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
//
// final IWorkbenchWindow window = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
// final IViewPart view = window.getActivePage().findView( FeatureTemplateView.ID );

    return Status.OK_STATUS;
  }

  /**
   * @see org.eclipse.core.runtime.IExecutableExtension#setInitializationData(org.eclipse.core.runtime.IConfigurationElement,
   *      java.lang.String, java.lang.Object)
   */
  public void setInitializationData( final IConfigurationElement config, final String propertyName, final Object data )
  {
  }
}
