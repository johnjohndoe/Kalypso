package org.kalypso.risk.model.actions.specificDamagePotential;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
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
import org.kalypso.ogc.gml.CascadingKalypsoTheme;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.MapModellHelper;
import org.kalypso.risk.model.actions.dataImport.waterdepth.Messages;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IVectorDataModel;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypso.ui.views.map.MapView;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

public class DamagePotentialCalculationHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent arg0 ) throws ExecutionException
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
        final IRasterDataModel model = scenarioDataProvider.getModel( IRasterDataModel.class );
        if( model.getWaterlevelCoverageCollection().size() == 0 )
        {
          MessageDialog.openError( shell, Messages.getString( "DamagePotentialCalculationHandler.5" ), Messages.getString( "DamagePotentialCalculationHandler.6" ) ); //$NON-NLS-1$ //$NON-NLS-2$
          return null;
        }
        for( final IAnnualCoverageCollection collection : model.getWaterlevelCoverageCollection() )
        {
          final Integer returnPeriod = collection.getReturnPeriod();
          if( returnPeriod == null || returnPeriod <= 0 )
          {
            MessageDialog.openError( shell, Messages.getString( "DamagePotentialCalculationHandler.5" ), Messages.getString( "DamagePotentialCalculationHandler.18" ) ); //$NON-NLS-1$ //$NON-NLS-2$
            return null;
          }
        }
        final IVectorDataModel vectorDataModel = scenarioDataProvider.getModel( IVectorDataModel.class );
        final IRasterizationControlModel rasterizationControlModel = scenarioDataProvider.getModel( IRasterizationControlModel.class );
        if( rasterizationControlModel.getAssetValueClassesList().size() == 0 )
        {
          MessageDialog.openError( shell, Messages.getString( "DamagePotentialCalculationHandler.7" ), Messages.getString( "DamagePotentialCalculationHandler.8" ) ); //$NON-NLS-1$ //$NON-NLS-2$
          return null;
        }
        final IFeatureWrapperCollection<ILandusePolygon> polygonCollection = vectorDataModel.getLandusePolygonCollection();
        final GMLWorkspace workspace = scenarioDataProvider.getCommandableWorkSpace( IRasterDataModel.class );
        final MapPanel mapPanel = mapView.getMapPanel();

        /* wait for map to load */
        if( !MapModellHelper.waitForAndErrorDialog( shell, mapPanel, "Berechne Schadenspotential", "Fehler beim Öffnen der Karte" ) )
          return null;

        final GisTemplateMapModell mapModell = (GisTemplateMapModell) mapPanel.getMapModell();

        /* get cascading them that holds the damage layers */
        final CascadingKalypsoTheme parentKalypsoTheme = RiskModelHelper.getCascadingTheme( mapModell, "Schadenspotentiale" );

        /* delete existing damage layers */
        RiskModelHelper.deleteExistingDamagePotentialMapLayers( parentKalypsoTheme );

        parentKalypsoTheme.setVisible( true );

        model.getSpecificDamageCoverageCollection().clear();

        new ProgressMonitorDialog( shell ).run( true, false, new IRunnableWithProgress()
        {
          public void run( final IProgressMonitor monitor ) throws InterruptedException
          {
            monitor.beginTask( Messages.getString( "DamagePotentialCalculationHandler.9" ), IProgressMonitor.UNKNOWN ); //$NON-NLS-1$
            try
            {
              for( final IAnnualCoverageCollection srcAnnualCoverages : model.getWaterlevelCoverageCollection() )
              {
                monitor.subTask( Messages.getString( "DamagePotentialCalculationHandler.10" ) + srcAnnualCoverages.getReturnPeriod() ); //$NON-NLS-1$

                final IFeatureWrapperCollection<IAnnualCoverageCollection> specificDamageCoverageCollection = model.getSpecificDamageCoverageCollection();
                // TODO: check if still ok: probably delete all underlying grids

                // remove existing (invalid) coverages from the model
                // specificDamageCoverageCollection.clear();
                // final List<IAnnualCoverageCollection> coveragesToRemove = new ArrayList<IAnnualCoverageCollection>();
                // for( final IAnnualCoverageCollection existingAnnualCoverage : specificDamageCoverageCollection )
                // if( existingAnnualCoverage.getReturnPeriod() == srcAnnualCoverages.getReturnPeriod() )
                // coveragesToRemove.add( existingAnnualCoverage );
                // for( final IAnnualCoverageCollection coverageToRemove : coveragesToRemove )
                // specificDamageCoverageCollection.remove( coverageToRemove );

                /* create annual damage coverage collection */
                final IAnnualCoverageCollection dstAnnualCoverages = RiskModelHelper.createAnnualDamageCoverages( scenarioFolder, polygonCollection, srcAnnualCoverages, specificDamageCoverageCollection );

                /* add the coverage collection to the map */
                RiskModelHelper.createDamagePotentialMapLayer( monitor, parentKalypsoTheme, dstAnnualCoverages, scenarioFolder );

                /* fireModellEvent to redraw a map */
                workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, model.getSpecificDamageCoverageCollection().getFeature(), new Feature[] { dstAnnualCoverages.getFeature() }, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
              }

              scenarioDataProvider.postCommand( IRasterDataModel.class, new EmptyCommand( "Get dirty!", false ) ); //$NON-NLS-1$

              /* Undoing this operation is not possible because old raster files are deleted */
              scenarioDataProvider.saveModel( new NullProgressMonitor() );
            }
            catch( final Exception e )
            {
              e.printStackTrace();
              throw new InterruptedException( e.getLocalizedMessage() );
            }
          }
        } );
        ;

        final IFile sldFile = scenarioFolder.getFile( "/styles/SpecificDamagePotentialCoverage.sld" ); //$NON-NLS-1$
        RiskModelHelper.updateDamageStyle( sldFile, model );

        if( mapView != null )
          mapPanel.invalidateMap();
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }
    return null;
  }

}
