package org.kalypso.risk.model.actions.riskZonesCalculation;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.risk.Messages;
import org.kalypso.risk.model.simulation.SimulationKalypsoRiskModelspecHelper;
import org.kalypso.risk.model.simulation.ISimulationSpecKalypsoRisk.SIMULATION_KALYPSORISK_TYPEID;
import org.kalypso.simulation.ui.calccase.ModelNature;
import org.kalypso.ui.views.map.MapView;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

public class RiskZonesCalculationHandler extends AbstractHandler
{
  public Object execute( final ExecutionEvent arg0 )
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final Shell shell = workbench.getDisplay().getActiveShell();
    final MapView mapView = (MapView) workbench.getActiveWorkbenchWindow().getActivePage().findView( MapView.ID );
    if( mapView == null )
    {
      StatusUtilities.createWarningStatus( Messages.getString( "RiskZonesCalculationHandler.0" ) ); //$NON-NLS-1$
      return false;
    }

    final Dialog dialog = new MessageDialog( shell, Messages.getString( "RiskZonesCalculationHandler.1" ), null, Messages.getString( "RiskZonesCalculationHandler.2" ), MessageDialog.QUESTION, new String[] { Messages.getString( "RiskZonesCalculationHandler.3" ), Messages.getString( "RiskZonesCalculationHandler.4" ) }, 0 ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    if( dialog.open() != 0 )
      return null;

    try
    {
      final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
      final IEvaluationContext context = handlerService.getCurrentState();
      final IFolder scenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );

      final Job job = new Job( Messages.getString( "RiskZonesCalculationHandler.8" ) )
      {
        @Override
        protected IStatus run( final IProgressMonitor monitor )
        {
          final IStatus status;
          try
          {
            status = ModelNature.runCalculation( scenarioFolder, monitor, SimulationKalypsoRiskModelspecHelper.getModeldata( SIMULATION_KALYPSORISK_TYPEID.RISK_ZONES_CALCULATION ) );
          }
          catch( final Exception e )
          {
            ErrorDialog.openError( shell, Messages.getString( "RiskZonesCalculationHandler.5" ), e.getLocalizedMessage(), Status.CANCEL_STATUS ); //$NON-NLS-1$
            return Status.CANCEL_STATUS;
          }
          return status;
        }
      };
      job.setUser( true );
      job.schedule( 100 );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return null;
  }
}
