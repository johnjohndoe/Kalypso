package org.kalypso.risk.model.actions.riskZonesCalculation;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.progress.IProgressService;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.simulation.ISimulationSpecKalypsoRisk.SIMULATION_KALYPSORISK_TYPEID;
import org.kalypso.risk.model.simulation.SimulationKalypsoRiskModelspecHelper;
import org.kalypso.simulation.ui.calccase.simulation.SimulationFactory;
import org.kalypso.ui.views.map.MapView;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

public class RiskZonesCalculationHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent arg0 )
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final Shell shell = workbench.getDisplay().getActiveShell();
    final MapView mapView = (MapView) workbench.getActiveWorkbenchWindow().getActivePage().findView( MapView.ID );
    if( mapView == null )
    {
      StatusUtilities.createWarningStatus( Messages.getString( "org.kalypso.risk.model.actions.riskZonesCalculation.RiskZonesCalculationHandler.0" ) ); //$NON-NLS-1$
      return false;
    }

    final Dialog dialog = new MessageDialog( shell, Messages.getString( "org.kalypso.risk.model.actions.riskZonesCalculation.RiskZonesCalculationHandler.1" ), null, Messages.getString( "org.kalypso.risk.model.actions.riskZonesCalculation.RiskZonesCalculationHandler.2" ), MessageDialog.QUESTION, new String[] { Messages.getString( "org.kalypso.risk.model.actions.riskZonesCalculation.RiskZonesCalculationHandler.3" ), Messages.getString( "org.kalypso.risk.model.actions.riskZonesCalculation.RiskZonesCalculationHandler.4" ) }, 0 ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    if( dialog.open() != 0 )
      return null;

    try
    {
      final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
      final IEvaluationContext context = handlerService.getCurrentState();
      final IFolder scenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );

      final IRunnableWithProgress operation = new IRunnableWithProgress()
      {
        @Override
        public void run( final IProgressMonitor monitor )
        {
          try
          {
            SimulationFactory.runCalculation( scenarioFolder, monitor, SimulationKalypsoRiskModelspecHelper.getModeldata( SIMULATION_KALYPSORISK_TYPEID.RISK_ZONES_CALCULATION ) );
          }
          catch( final CoreException e )
          {
            ErrorDialog.openError( shell, Messages.getString( "org.kalypso.risk.model.actions.riskZonesCalculation.RiskZonesCalculationHandler.5" ), e.getLocalizedMessage(), Status.CANCEL_STATUS ); //$NON-NLS-1$
          }
        }
      };
      final IProgressService service = PlatformUI.getWorkbench().getProgressService();
      service.run( true, false, operation );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return null;
  }
}
