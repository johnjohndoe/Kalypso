package org.kalypso.risk.model.actions.specificDamage;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
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
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.simulation.ISimulationSpecKalypsoRisk.SIMULATION_KALYPSORISK_TYPEID;
import org.kalypso.risk.model.simulation.SimulationKalypsoRiskModelspecHelper;
import org.kalypso.risk.plugin.KalypsoRiskPlugin;
import org.kalypso.simulation.core.simspec.Modeldata;
import org.kalypso.simulation.ui.calccase.simulation.SimulationFactory;
import org.kalypso.ui.views.map.MapView;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

public class SpecificDamageCalculationHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent arg0 )
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final Shell shell = workbench.getDisplay().getActiveShell();
    final MapView mapView = (MapView) workbench.getActiveWorkbenchWindow().getActivePage().findView( MapView.ID );

    if( mapView == null )
    {
      StatusUtilities.createWarningStatus( Messages.getString( "org.kalypso.risk.model.actions.specificDamage.DamagePotentialCalculationHandler.0" ) ); //$NON-NLS-1$
      return false;
    }

    final Dialog dialog = new MessageDialog( shell, Messages.getString( "org.kalypso.risk.model.actions.specificDamage.DamagePotentialCalculationHandler.1" ), null, Messages.getString( "org.kalypso.risk.model.actions.specificDamage.DamagePotentialCalculationHandler.2" ), MessageDialog.QUESTION, new String[] { Messages.getString( "org.kalypso.risk.model.actions.specificDamage.DamagePotentialCalculationHandler.3" ), Messages.getString( "org.kalypso.risk.model.actions.specificDamage.DamagePotentialCalculationHandler.4" ) }, 0 ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    if( dialog.open() == 0 )
    {
      final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
      final IEvaluationContext context = handlerService.getCurrentState();
      final SzenarioDataProvider scenarioDataProvider = (SzenarioDataProvider) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
      final IFolder scenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
      try
      {
        final IRasterizationControlModel rasterizationControlModel = scenarioDataProvider.getModel( IRasterizationControlModel.MODEL_ID, IRasterizationControlModel.class );

        if( rasterizationControlModel.getAssetValueClassesList().size() == 0 )
        {
          MessageDialog.openError( shell, Messages.getString( "org.kalypso.risk.model.actions.specificDamage.DamagePotentialCalculationHandler.7" ), Messages.getString( "org.kalypso.risk.model.actions.specificDamage.DamagePotentialCalculationHandler.8" ) ); //$NON-NLS-1$ //$NON-NLS-2$
          return null;
        }

        final IRunnableWithProgress operation = new IRunnableWithProgress()
        {
          @Override
          public void run( final IProgressMonitor monitor )
          {
            try
            {
              final Modeldata modeldata = SimulationKalypsoRiskModelspecHelper.getModeldata( SIMULATION_KALYPSORISK_TYPEID.SPECIFIC_DAMAGE_CALCULATION );
              SimulationFactory.runCalculation( scenarioFolder, monitor, modeldata );
            }
            catch( final CoreException e )
            {
              ErrorDialog.openError( shell, Messages.getString( "org.kalypso.risk.model.actions.specificDamage.SpecificDamageCalculationHandler.0" ), e.getLocalizedMessage(), Status.CANCEL_STATUS ); //$NON-NLS-1$
            }
          }
        };

        final IProgressService service = PlatformUI.getWorkbench().getProgressService();
        service.run( true, false, operation );
      }
      catch( final Exception e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        KalypsoRiskPlugin.getDefault().getLog().log( status );
        ErrorDialog.openError( shell, "", Messages.getString( "org.kalypso.risk.model.actions.specificDamage.SpecificDamageCalculationHandler.5" ), status ); //$NON-NLS-1$ //$NON-NLS-2$
        return null;
      }
    }
    return null;
  }

}
