package org.kalypso.risk.model.actions.riskZonesCalculation;

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
import org.kalypso.risk.Messages;
import org.kalypso.risk.model.operation.RiskCalcRiskZonesRunnable;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IVectorDataModel;
import org.kalypso.risk.plugin.KalypsoRiskPlugin;
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
      final SzenarioDataProvider scenarioDataProvider = (SzenarioDataProvider) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
      final IFolder scenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
      final IRasterizationControlModel controlModel = scenarioDataProvider.getModel( IRasterizationControlModel.class );
      final IRasterDataModel rasterModel = scenarioDataProvider.getModel( IRasterDataModel.class );
      final IVectorDataModel vectorModel = scenarioDataProvider.getModel( IVectorDataModel.class );

      final ICoreRunnableWithProgress riskCalcRiskZonesRunnable = new RiskCalcRiskZonesRunnable( rasterModel, vectorModel, controlModel, scenarioFolder );

      final IStatus execute = RunnableContextHelper.execute( new ProgressMonitorDialog( shell ), true, true, riskCalcRiskZonesRunnable );
      ErrorDialog.openError( shell, Messages.getString( "RiskZonesCalculationHandler.5" ), Messages.getString( "RiskZonesCalculationHandler.6" ), execute ); //$NON-NLS-1$ //$NON-NLS-2$

      if( !execute.isOK() )
      {
        KalypsoRiskPlugin.getDefault().getLog().log( execute );
      }

      scenarioDataProvider.postCommand( IRasterDataModel.class, new EmptyCommand( "Get dirty!", false ) ); //$NON-NLS-1$
      scenarioDataProvider.postCommand( IRasterizationControlModel.class, new EmptyCommand( "Get dirty!", false ) ); //$NON-NLS-1$

      // Undoing this operation is not possible because old raster files are deleted...
      scenarioDataProvider.saveModel( new NullProgressMonitor() );

      mapView.getMapPanel().invalidateMap();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return null;
  }
}
