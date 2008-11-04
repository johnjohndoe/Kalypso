package org.kalypso.risk.model.actions.riskZonesCalculation;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.risk.Messages;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel.RISK_CALCULATION_TYPE;
import org.kalypso.simulation.ui.calccase.CalcCaseJob;
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
      final SzenarioDataProvider scenarioDataProvider = (SzenarioDataProvider) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
      final IFolder scenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
      final IRasterizationControlModel controlModel = scenarioDataProvider.getModel( IRasterizationControlModel.class );

      // set calculation type flag
      controlModel.setRiskCalculationType( RISK_CALCULATION_TYPE.RISK_ZONES_CALCULATION );
      scenarioDataProvider.postCommand( IRasterizationControlModel.class, new EmptyCommand( "Just to make it dirty.", false ) ); //$NON-NLS-1$
      scenarioDataProvider.saveModel( IRasterizationControlModel.class, new NullProgressMonitor() );

      new CalcCaseJob( scenarioFolder ).schedule();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return null;
  }
}
