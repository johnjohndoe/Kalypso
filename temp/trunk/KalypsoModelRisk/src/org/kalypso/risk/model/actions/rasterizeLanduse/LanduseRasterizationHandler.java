package org.kalypso.risk.model.actions.rasterizeLanduse;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.risk.Messages;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel.RISK_CALCULATION_TYPE;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypso.simulation.ui.calccase.CalcCaseJob;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

public class LanduseRasterizationHandler extends AbstractHandler
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
      final IRasterDataModel rasterModel = scenarioDataProvider.getModel( IRasterDataModel.class );

      final IFeatureWrapperCollection<IAnnualCoverageCollection> waterDepthCoverageCollection = rasterModel.getWaterlevelCoverageCollection();

      if( waterDepthCoverageCollection.size() == 0 )
      {
        ErrorDialog.openError( shell, Messages.getString( "LanduseRasterizationHandler.0" ), Messages.getString( "LanduseRasterizationHandler.1" ), Status.CANCEL_STATUS ); //$NON-NLS-1$ //$NON-NLS-2$
        return null;
      }
      final IAnnualCoverageCollection maxCoveragesCollection = RiskModelHelper.getMaxReturnPeriodCollection( waterDepthCoverageCollection );
      final Integer maxReturnPeriod = maxCoveragesCollection.getReturnPeriod();

      if( maxReturnPeriod == Integer.MIN_VALUE )
        return StatusUtilities.createErrorStatus( Messages.getString( "LanduseRasterizationHandler.2" ) ); //$NON-NLS-1$

      /* info dialog, that the rasterisation is don by using the extend of the grid with the max return period */
      final String dialogTitle = Messages.getString( "LanduseRasterizationHandler.3" ); //$NON-NLS-1$
      final String dialogMessage = Messages.getString( "LanduseRasterizationHandler.4" ) + maxReturnPeriod + Messages.getString( "LanduseRasterizationHandler.5" ); //$NON-NLS-1$ //$NON-NLS-2$

      final Dialog dialog = new MessageDialog( shell, dialogTitle, null, dialogMessage, MessageDialog.QUESTION, new String[] { "Ja", "Nein" }, 0 ); //$NON-NLS-1$ //$NON-NLS-2$

      if( dialog.open() != 0 )
        return null;
      
      // set calculation type flag
      final IRasterizationControlModel controlModel = scenarioDataProvider.getModel( IRasterizationControlModel.class );
      controlModel.setRiskCalculationType( RISK_CALCULATION_TYPE.LANDUSE_RASTERIZATION );
      scenarioDataProvider.postCommand( IRasterizationControlModel.class, new EmptyCommand( "Just to make it dirty.", false ) ); //$NON-NLS-1$
      scenarioDataProvider.saveModel( IRasterizationControlModel.class, new NullProgressMonitor() );

      // run calculation (as WPS)
      new CalcCaseJob( scenarioFolder ).schedule();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    return null;
  }
}
