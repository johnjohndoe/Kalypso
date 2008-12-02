package org.kalypso.risk.model.actions.specificDamage;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
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
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypso.risk.Messages;
import org.kalypso.risk.model.operation.RiskCalcSpecificDamageRunnable;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IVectorDataModel;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypso.risk.plugin.KalypsoRiskPlugin;
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
      StatusUtilities.createWarningStatus( Messages.getString( "DamagePotentialCalculationHandler.0" ) ); //$NON-NLS-1$
      return false;
    }

    final Dialog dialog = new MessageDialog( shell, Messages.getString( "DamagePotentialCalculationHandler.1" ), null, Messages.getString( "DamagePotentialCalculationHandler.2" ), MessageDialog.QUESTION, new String[] { Messages.getString( "DamagePotentialCalculationHandler.3" ), Messages.getString( "DamagePotentialCalculationHandler.4" ) }, 0 ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    if( dialog.open() == 0 )
    {
      try
      {
        final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
        final IEvaluationContext context = handlerService.getCurrentState();
        final SzenarioDataProvider scenarioDataProvider = (SzenarioDataProvider) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
        final IFolder scenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );

        final IRasterDataModel rasterDataModel = scenarioDataProvider.getModel( IRasterDataModel.class );
        final IVectorDataModel vectorDataModel = scenarioDataProvider.getModel( IVectorDataModel.class );
        final IRasterizationControlModel rasterizationControlModel = scenarioDataProvider.getModel( IRasterizationControlModel.class );

        if( rasterizationControlModel.getAssetValueClassesList().size() == 0 )
        {
          MessageDialog.openError( shell, Messages.getString( "DamagePotentialCalculationHandler.7" ), Messages.getString( "DamagePotentialCalculationHandler.8" ) ); //$NON-NLS-1$ //$NON-NLS-2$
          return null;
        }

        final MapPanel mapPanel = mapView.getMapPanel();

        /* wait for map to load */
        if( !MapModellHelper.waitForAndErrorDialog( shell, mapPanel, org.kalypso.risk.Messages.getString( "SpecificDamageCalculationHandler.0" ), org.kalypso.risk.Messages.getString( "SpecificDamageCalculationHandler.1" ) ) ) //$NON-NLS-1$ //$NON-NLS-2$
          return null;

        final GisTemplateMapModell mapModell = (GisTemplateMapModell) mapPanel.getMapModell();

        final ICoreRunnableWithProgress runnableWithProgress = new RiskCalcSpecificDamageRunnable( rasterizationControlModel, rasterDataModel, vectorDataModel, scenarioFolder );

        final IStatus execute = RunnableContextHelper.execute( new ProgressMonitorDialog( shell ), true, true, runnableWithProgress );
        ErrorDialog.openError( shell, org.kalypso.risk.Messages.getString( "SpecificDamageCalculationHandler.2" ), org.kalypso.risk.Messages.getString( "SpecificDamageCalculationHandler.3" ), execute ); //$NON-NLS-1$ //$NON-NLS-2$

        if( !execute.isOK() )
          KalypsoRiskPlugin.getDefault().getLog().log( execute );

        scenarioDataProvider.postCommand( IRasterDataModel.class, new EmptyCommand( "Get dirty!", false ) ); //$NON-NLS-1$
        scenarioDataProvider.postCommand( IRasterizationControlModel.class, new EmptyCommand( "Get dirty!", false ) ); //$NON-NLS-1$

        /* Undoing this operation is not possible because old raster files are deleted */
        scenarioDataProvider.saveModel( new NullProgressMonitor() );

        RiskModelHelper.updateDamageLayers( scenarioFolder, rasterDataModel, mapModell );

        if( mapView != null )
          mapPanel.invalidateMap();
      }
      catch( final Exception e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        KalypsoRiskPlugin.getDefault().getLog().log( status );

        ErrorDialog.openError( shell, org.kalypso.risk.Messages.getString( "SpecificDamageCalculationHandler.4" ), org.kalypso.risk.Messages.getString( "SpecificDamageCalculationHandler.5" ), status ); //$NON-NLS-1$ //$NON-NLS-2$
      }
    }
    return null;
  }

}
