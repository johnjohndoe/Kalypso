package org.kalypso.risk.model.actions.rasterizeLanduse;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.risk.model.operation.RiskLanduseRasterizationRunnable;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.schema.binding.IVectorDataModel;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypso.risk.plugin.KalypsoRiskPlugin;
import org.kalypso.ui.views.map.MapView;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

public class LanduseRasterizationHandler extends AbstractHandler
{
  @Override
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
      final IRasterDataModel rasterModel = scenarioDataProvider.getModel( IRasterDataModel.class );
      final IVectorDataModel vectorDataModel = scenarioDataProvider.getModel( IVectorDataModel.class );

      final IFeatureWrapperCollection<IAnnualCoverageCollection> waterDepthCoverageCollection = rasterModel.getWaterlevelCoverageCollection();

      if( waterDepthCoverageCollection.size() == 0 )
      {
        ErrorDialog.openError( shell, "Fehler", "Keine Fliesstiefen Rasterdaten vorhanden. Bitte importieren Sie zuerst die Fliesstiefen.", Status.CANCEL_STATUS );
        return null;
      }
      final IAnnualCoverageCollection maxCoveragesCollection = RiskModelHelper.getMaxReturnPeriodCollection( waterDepthCoverageCollection );
      final Integer maxReturnPeriod = maxCoveragesCollection.getReturnPeriod();

      if( maxReturnPeriod == Integer.MIN_VALUE )
        return StatusUtilities.createErrorStatus( "Missing HQ data. No waterlevel data loaded. Please load waterlevel raster data before rasterizing landuse classes." );

      /* info dialog, that the rasterisation is don by using the extend of the grid with the max return period */
      final String dialogTitle = "Rasterung der Landnutzung";
      final String dialogMessage = "Erstellung der Landnutzungsraster (Basisraster ist HQ" + maxReturnPeriod + ")?";

      final Dialog dialog = new MessageDialog( shell, dialogTitle, null, dialogMessage, MessageDialog.QUESTION, new String[] { "Ja", "Nein" }, 0 ); //$NON-NLS-1$ //$NON-NLS-2$

      if( dialog.open() != 0 )
        return null;

      final ICoreRunnableWithProgress runnableWithProgress = new RiskLanduseRasterizationRunnable( rasterModel, vectorDataModel, scenarioFolder );

      IStatus execute = RunnableContextHelper.execute( new ProgressMonitorDialog( shell ), true, false, runnableWithProgress );
      ErrorDialog.openError( shell, "Fehler", "Fehler bei der Rasterung der Landnutzung", execute );

      if( !execute.isOK() )
      {
        KalypsoRiskPlugin.getDefault().getLog().log( execute );
      }

      scenarioDataProvider.postCommand( IRasterDataModel.class, new EmptyCommand( "Get dirty!", false ) ); //$NON-NLS-1$

      /* Undoing this operation is not possible because old raster files are deleted */
      scenarioDataProvider.saveModel( new NullProgressMonitor() );

      final MapView mapView = (MapView) workbench.getActiveWorkbenchWindow().getActivePage().findView( MapView.ID );
      if( mapView != null )
        mapView.getMapPanel().invalidateMap();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return null;
  }
}
