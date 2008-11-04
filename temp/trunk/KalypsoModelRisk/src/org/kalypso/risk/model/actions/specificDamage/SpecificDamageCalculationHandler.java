package org.kalypso.risk.model.actions.specificDamage;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
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
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.risk.Messages;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel.RISK_CALCULATION_TYPE;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypso.risk.plugin.KalypsoRiskPlugin;
import org.kalypso.simulation.ui.calccase.CalcCaseJob;
import org.kalypso.ui.views.map.MapView;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

public class SpecificDamageCalculationHandler extends AbstractHandler
{
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
      final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
      final IEvaluationContext context = handlerService.getCurrentState();
      final SzenarioDataProvider scenarioDataProvider = (SzenarioDataProvider) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
      final IFolder scenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
      try
      {
        final IRasterizationControlModel controlModel = scenarioDataProvider.getModel( IRasterizationControlModel.class );
        final IRasterizationControlModel rasterizationControlModel = scenarioDataProvider.getModel( IRasterizationControlModel.class );

        if( rasterizationControlModel.getAssetValueClassesList().size() == 0 )
        {
          MessageDialog.openError( shell, Messages.getString( "DamagePotentialCalculationHandler.7" ), Messages.getString( "DamagePotentialCalculationHandler.8" ) ); //$NON-NLS-1$ //$NON-NLS-2$
          return null;
        }
        // set calculation type flag
        controlModel.setRiskCalculationType( RISK_CALCULATION_TYPE.DAMAGE_POTENTIAL_CALCULATION );
        scenarioDataProvider.postCommand( IRasterizationControlModel.class, new EmptyCommand( "Just to make it dirty.", false ) ); //$NON-NLS-1$
        scenarioDataProvider.saveModel( IRasterizationControlModel.class, new NullProgressMonitor() );
      }
      catch( final Exception e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        KalypsoRiskPlugin.getDefault().getLog().log( status );
        ErrorDialog.openError( shell, org.kalypso.risk.Messages.getString( "SpecificDamageCalculationHandler.4" ), org.kalypso.risk.Messages.getString( "SpecificDamageCalculationHandler.5" ), status ); //$NON-NLS-1$ //$NON-NLS-2$
        return null;
      }

      final IMapPanel mapPanel = mapView.getMapPanel();

      // run calculation (as WPS)
      final CalcCaseJob calcJob = new CalcCaseJob( scenarioFolder );
      calcJob.addJobChangeListener( new JobChangeAdapter()
      {
        /**
         * @see org.eclipse.core.runtime.jobs.JobChangeAdapter#done(org.eclipse.core.runtime.jobs.IJobChangeEvent)
         */
        @Override
        public void done( final IJobChangeEvent event )
        {
          if( event.getResult().isOK() )
          {
            try
            {
              /* wait for map to load */
              while( !mapPanel.getMapModell().isLoaded() )
                Thread.sleep( 300 );
              final GisTemplateMapModell mapModell = (GisTemplateMapModell) mapPanel.getMapModell();
              final IRasterDataModel rasterDataModel = scenarioDataProvider.getModel( IRasterDataModel.class );
              final IFile sldFile = scenarioFolder.getFile( "/styles/SpecificDamagePotentialCoverage.sld" ); //$NON-NLS-1$
              final IFeatureWrapperCollection<IAnnualCoverageCollection> specificDamageCoverageCollection = rasterDataModel.getSpecificDamageCoverageCollection();
              RiskModelHelper.updateDamageStyle( sldFile, specificDamageCoverageCollection );
              RiskModelHelper.updateDamageLayers( scenarioFolder, specificDamageCoverageCollection, mapModell );
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
        }
      } );
      calcJob.schedule();
    }
    return null;
  }

}
