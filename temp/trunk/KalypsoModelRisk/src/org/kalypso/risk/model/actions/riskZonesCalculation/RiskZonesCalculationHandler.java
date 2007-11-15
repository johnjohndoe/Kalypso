package org.kalypso.risk.model.actions.riskZonesCalculation;

import java.io.File;
import java.util.Date;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IVectorDataModel;
import org.kalypso.ui.views.map.MapView;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

public class RiskZonesCalculationHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent arg0 ) throws ExecutionException
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final Shell shell = workbench.getDisplay().getActiveShell();
    final MapView mapView = (MapView) workbench.getActiveWorkbenchWindow().getActivePage().findView( MapView.ID );
    if( mapView == null )
    {
      StatusUtilities.createWarningStatus( "Kartenansicht nicht ge�ffnet. Es k�nnen keine Themen hinzugef�gt werden." );
      return false;
    }
    final Dialog dialog = new MessageDialog( shell, "Rasterizing risk zones", null, "Do you want to calcualte risk zones?", MessageDialog.QUESTION, new String[] { "Ja", "Nein" }, 0 );
    if( dialog.open() == 0 )
    {
      try
      {
        final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
        final IEvaluationContext context = handlerService.getCurrentState();
        final SzenarioDataProvider scenarioDataProvider = (SzenarioDataProvider) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
        final IFolder scenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
        final IRasterizationControlModel controlModel = scenarioDataProvider.getModel( IRasterizationControlModel.class );
        final IRasterDataModel rasterModel = scenarioDataProvider.getModel( IRasterDataModel.class );
        final IVectorDataModel vectorModel = scenarioDataProvider.getModel( IVectorDataModel.class );
        if( rasterModel.getSpecificDamageCoverageCollection().size() < 2 )
        {
          MessageDialog.openError( shell, "Error", "Risk zones calculation cannot be started. Not enough specific damage potentials are calculated. To start risk zones calculation, al least two specific damage potentials should be available." );
          return null;
        }
        IAnnualCoverageCollection maxCoveragesCollection = null;
        int maxReturnPeriod = Integer.MIN_VALUE;
        for( final IAnnualCoverageCollection annualCoverageCollection : rasterModel.getSpecificDamageCoverageCollection() )
        {
          if( annualCoverageCollection.getReturnPeriod() > maxReturnPeriod )
          {
            maxReturnPeriod = annualCoverageCollection.getReturnPeriod();
            maxCoveragesCollection = annualCoverageCollection;
          }
        }
        // remove existing (invalid) coverages from the model
        rasterModel.getRiskZonesCoverage().clear();

        final GMLWorkspace workspace = scenarioDataProvider.getCommandableWorkSpace( IRasterDataModel.class );
        final ICoverageCollection baseCoverages = maxCoveragesCollection;
        new ProgressMonitorDialog( shell ).run( true, false, new IRunnableWithProgress()
        {
          public void run( final IProgressMonitor monitor ) throws InterruptedException
          {
            monitor.beginTask( "Calculating risk zones", IProgressMonitor.UNKNOWN );
            try
            {
              final ICoverageCollection outputCoverages = rasterModel.getRiskZonesCoverage();
              int count = 0;
              for( final ICoverage srcSpecificDamageCoverage : baseCoverages )
              {
                final IGeoGrid inputGrid = GeoGridUtilities.toGrid( srcSpecificDamageCoverage );
                final IGeoGrid outputGrid = new RiskZonesGrid( inputGrid, rasterModel.getSpecificDamageCoverageCollection(), vectorModel.getLandusePolygonCollection(), controlModel.getLanduseClassesList(), controlModel.getRiskZoneDefinitionsList() );
                // TODO: change name: better: use input name
                final String outputFilePath = "raster/output/RiskZonesCoverage" + count + ".dat"; //$NON-NLS-1$ //$NON-NLS-2$

                final IFile ifile = scenarioFolder.getFile( new Path( "models/" + outputFilePath ) ); //$NON-NLS-1$
                final File file = new File( ifile.getRawLocation().toPortableString() );
                final ICoverage coverage = GeoGridUtilities.addCoverage( outputCoverages, outputGrid, file, outputFilePath, "image/bin", new NullProgressMonitor() ); //$NON-NLS-1$
                inputGrid.dispose();

                coverage.setName( "RiskZonesCoverage_" + count );
                coverage.setDescription( "Created on " + new Date().toString() );
                count++;

                // fireModellEvent to redraw a map...
                workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, rasterModel.getSpecificDamageCoverageCollection().getWrappedFeature(), new Feature[] { outputCoverages.getWrappedFeature() }, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
              }

              scenarioDataProvider.postCommand( IRasterDataModel.class, new EmptyCommand( "Get dirty!", false ) ); //$NON-NLS-1$

              // statistics...
              scenarioDataProvider.postCommand( IRasterizationControlModel.class, new EmptyCommand( "Get dirty!", false ) ); //$NON-NLS-1$

              // Undoing this operation is not possible because old raster files are deleted...
              scenarioDataProvider.saveModel( new NullProgressMonitor() );
            }
            catch( final Exception e )
            {
              e.printStackTrace();
              throw new InterruptedException( e.getLocalizedMessage() );
            }
          }
        } );

        if( mapView != null )
          mapView.getMapPanel().invalidateMap();
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }

    }
    return null;
  }

}
