package org.kalypso.risk.model.actions.rasterizeLanduse;

import java.io.File;
import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
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
import org.kalypso.grid.AbstractDelegatingGeoGrid;
import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.schema.binding.IVectorDataModel;
import org.kalypso.ui.views.map.MapView;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

public class LanduseRasterizationHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent arg0 ) throws ExecutionException
  {
    final Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    final SzenarioDataProvider scenarioDataProvider = (SzenarioDataProvider) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
    final IFolder scenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
    final IRasterDataModel model;
    try
    {
      model = scenarioDataProvider.getModel( IRasterDataModel.class );
    }
    catch( CoreException e1 )
    {
      e1.printStackTrace();
      MessageDialog.openError( shell, "Error", "Raster data model cannot be loaded." );
      return null;
    }

    if( model.getWaterlevelCoverageCollection().size() == 0 )
    {
      MessageDialog.openError( shell, "Error", "No HQ data is loaded. Landuse rasterisation is still not possible. Please load waterlevel raster data." );
      return null;
    }

    IAnnualCoverageCollection maxCoveragesCollection = null;
    int maxReturnPeriod = Integer.MIN_VALUE;
    for( final IAnnualCoverageCollection annualCoverageCollection : model.getWaterlevelCoverageCollection() )
    {
      if( annualCoverageCollection.getReturnPeriod() > maxReturnPeriod )
      {
        maxReturnPeriod = annualCoverageCollection.getReturnPeriod();
        maxCoveragesCollection = annualCoverageCollection;
      }
    }

    final String dialogTitle = "Rasterize landuse";
    final String dialogMessage = "Do you want to create landuse raster (base raster is HQ " + maxReturnPeriod + ")?";
    final Dialog dialog = new MessageDialog( shell, dialogTitle, null, dialogMessage, MessageDialog.QUESTION, new String[] { "Ja", "Nein" }, 0 );
    if( dialog.open() == 0 )
    {
      try
      {
        final ICoverageCollection inputCoverages = maxCoveragesCollection;
        final ICoverageCollection outputCoverages = model.getLanduseCoverage();

        // remove existing (invalid) coverages from the model
        model.getRiskZonesCoverage().clear();

        final GMLWorkspace workspace = scenarioDataProvider.getCommandableWorkSpace( IRasterDataModel.class );
        new ProgressMonitorDialog( shell ).run( true, false, new IRunnableWithProgress()
        {
          public void run( final IProgressMonitor monitor ) throws InterruptedException
          {
            monitor.beginTask( "Rasterizing landuse vector data...", IProgressMonitor.UNKNOWN );
            try
            {
              final IVectorDataModel vectorDataModel = scenarioDataProvider.getModel( IVectorDataModel.class );
              final IFeatureWrapperCollection<ILandusePolygon> polygonCollection = vectorDataModel.getLandusePolygonCollection();

              // TODO: delete old landuse coverage (also the files!)

              int count = 0;
              for( final ICoverage inputCoverage : inputCoverages )
              {
                final IGeoGrid inputGrid = GeoGridUtilities.toGrid( inputCoverage );

                final IGeoGrid outputGrid = new AbstractDelegatingGeoGrid( inputGrid )
                {
                  /**
                   * @see org.kalypso.grid.AbstractDelegatingGeoGrid#getValue(int, int)
                   */
                  @Override
                  public double getValue( int x, int y ) throws GeoGridException
                  {
                    final Double value = super.getValue( x, y );
                    if( value.equals( Double.NaN ) )
                      return Double.NaN;
                    else
                    {
                      final Coordinate coordinate = GeoGridUtilities.toCoordinate( inputGrid, x, y, null );
                      final GM_Position positionAt = JTSAdapter.wrap( coordinate );
                      final List<ILandusePolygon> list = polygonCollection.query( positionAt );
                      if( list == null || list.size() == 0 )
                        return Double.NaN;
                      else
                        for( final ILandusePolygon polygon : list )
                        {
                          if( polygon.contains( positionAt ) )
                            return polygon.getLanduseClassOrdinalNumber();
                        }
                      return Double.NaN;
                    }
                  }
                };

                // TODO: change name: better: use input name
                final String outputFilePath = "raster/output/LanduseCoverage" + count + ".dat"; //$NON-NLS-1$

                final IFile ifile = scenarioFolder.getFile( new Path( "models/" + outputFilePath ) );
                final File file = new File( ifile.getRawLocation().toPortableString() );
                GeoGridUtilities.addCoverage( outputCoverages, outputGrid, file, outputFilePath, "image/bin", new NullProgressMonitor() ); //$NON-NLS-1$
                inputGrid.dispose();

                count++;
              }

              // fireModellEvent to redraw a map...
              // TODO: check if still ok
              workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, model.getWrappedFeature(), new Feature[] { outputCoverages.getWrappedFeature() }, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

              scenarioDataProvider.postCommand( IRasterDataModel.class, new EmptyCommand( "Get dirty!", false ) ); //$NON-NLS-1$

            }
            catch( final Exception e )
            {
              e.printStackTrace();
              throw new InterruptedException( e.getLocalizedMessage() );
            }
          }
        } );
        final MapView mapView = (MapView) workbench.getActiveWorkbenchWindow().getActivePage().findView( MapView.ID );
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
