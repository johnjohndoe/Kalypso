package org.kalypso.risk.model.actions.rasterizeLanduse;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
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
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.simulation.ISimulationSpecKalypsoRisk;
import org.kalypso.risk.model.simulation.SimulationKalypsoRiskModelspecHelper;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypso.simulation.ui.calccase.ModelNature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

public class LanduseRasterizationHandler extends AbstractHandler implements ISimulationSpecKalypsoRisk
{
  public Object execute( final ExecutionEvent arg0 )
  {
    final Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    final SzenarioDataProvider scenarioDataProvider = (SzenarioDataProvider) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
    final IFolder scenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );

    try
    {
      final IRasterDataModel rasterModel = scenarioDataProvider.getModel( IRasterDataModel.MODEL_ID, IRasterDataModel.class );
      final IFeatureWrapperCollection<IAnnualCoverageCollection> waterDepthCoverageCollection = rasterModel.getWaterlevelCoverageCollection();

      if( waterDepthCoverageCollection.size() == 0 )
      {
        ErrorDialog.openError( shell, Messages.getString( "org.kalypso.risk.model.actions.rasterizeLanduse.LanduseRasterizationHandler.0" ), Messages.getString( "org.kalypso.risk.model.actions.rasterizeLanduse.LanduseRasterizationHandler.1" ), Status.CANCEL_STATUS ); //$NON-NLS-1$ //$NON-NLS-2$
        return null;
      }
      final IAnnualCoverageCollection maxCoveragesCollection = RiskModelHelper.getMaxReturnPeriodCollection( waterDepthCoverageCollection );
      final Integer maxReturnPeriod = maxCoveragesCollection.getReturnPeriod();

      if( maxReturnPeriod == Integer.MIN_VALUE )
        return StatusUtilities.createErrorStatus( Messages.getString( "org.kalypso.risk.model.actions.rasterizeLanduse.LanduseRasterizationHandler.2" ) ); //$NON-NLS-1$

      /* info dialog, that the rasterization is done by using the extend of the grid with the max return period */
      final String dialogTitle = Messages.getString( "org.kalypso.risk.model.actions.rasterizeLanduse.LanduseRasterizationHandler.3" ); //$NON-NLS-1$
      final String dialogMessage = Messages.getString( "org.kalypso.risk.model.actions.rasterizeLanduse.LanduseRasterizationHandler.4" ) + maxReturnPeriod + Messages.getString( "org.kalypso.risk.model.actions.rasterizeLanduse.LanduseRasterizationHandler.5" ); //$NON-NLS-1$ //$NON-NLS-2$

      final Dialog dialog = new MessageDialog( shell, dialogTitle, null, dialogMessage, MessageDialog.QUESTION, new String[] { org.kalypso.risk.i18n.Messages.getString("org.kalypso.risk.model.actions.rasterizeLanduse.LanduseRasterizationHandler.6"), org.kalypso.risk.i18n.Messages.getString("org.kalypso.risk.model.actions.rasterizeLanduse.LanduseRasterizationHandler.7") }, 0 ); //$NON-NLS-1$ //$NON-NLS-2$

      if( dialog.open() != 0 )
        return null;

      final Job job = new Job(  Messages.getString("org.kalypso.risk.model.actions.rasterizeLanduse.LanduseRasterizationHandler.9") )  //$NON-NLS-1$
      {
        @Override
        protected IStatus run( final IProgressMonitor monitor )
        {
          final IStatus status;
          try
          {
            status = ModelNature.runCalculation( scenarioFolder, monitor, SimulationKalypsoRiskModelspecHelper.getModeldata( SIMULATION_KALYPSORISK_TYPEID.LANDUSE_RASTERIZATION ) );
          }
          catch( final Exception e )
          {
            ErrorDialog.openError( shell, Messages.getString( "org.kalypso.risk.model.actions.rasterizeLanduse.LanduseRasterizationHandler.0" ), e.getLocalizedMessage(), Status.CANCEL_STATUS ); //$NON-NLS-1$
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
