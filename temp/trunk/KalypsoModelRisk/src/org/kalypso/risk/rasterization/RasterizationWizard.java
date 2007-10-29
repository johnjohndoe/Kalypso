package org.kalypso.risk.rasterization;

import java.io.File;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.grid.AbstractDelegatingGeoGrid;
import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.risk.model.schema.binding.ILanduseCoverageModel;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.ILanduseVectorModel;
import org.kalypso.risk.model.schema.binding.IWaterdepthCoverage;
import org.kalypso.risk.model.schema.binding.IWaterdepthCoverageModel;
import org.kalypso.risk.model.wizards.importwaterdepth.Messages;
import org.kalypso.ui.views.map.MapView;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

public class RasterizationWizard extends Wizard implements INewWizard
{
  public void init( IWorkbench workbench, IStructuredSelection selection )
  {
    // TODO Auto-generated method stub

  }

  @Override
  public boolean performFinish( )
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final MapView mapView = (MapView) workbench.getActiveWorkbenchWindow().getActivePage().findView( MapView.ID );
    if( mapView == null )
    {
      StatusUtilities.createWarningStatus( "Kartenansicht nicht geöffnet. Es können keine Themen hinzugefügt werden." );
      return false;
    }
    final GisTemplateMapModell mapModell = (GisTemplateMapModell) mapView.getMapPanel().getMapModell();
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    final SzenarioDataProvider scenarioDataProvider = (SzenarioDataProvider) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
    final IFolder scenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
    try
    {
      final GMLWorkspace workspace = scenarioDataProvider.getCommandableWorkSpace( IWaterdepthCoverageModel.class );
      getContainer().run( true, true, new IRunnableWithProgress()
      {
        public void run( final IProgressMonitor monitor ) throws InterruptedException
        {
          monitor.beginTask( Messages.getString( "ImportWaterdepthWizard.1" ), IProgressMonitor.UNKNOWN ); //$NON-NLS-1$
          try
          {
            monitor.subTask( Messages.getString( "ImportWaterdepthWizard.7" ) ); //$NON-NLS-1$
            final IWaterdepthCoverageModel waterdepthCoverageModel = scenarioDataProvider.getModel( IWaterdepthCoverageModel.class );
            final IFeatureWrapperCollection<IWaterdepthCoverage> waterdepthCoverageCollection = waterdepthCoverageModel.getWaterdepthCoverageCollection();
            final ILanduseVectorModel landuseVectorModel = (ILanduseVectorModel) scenarioDataProvider.getModel( ILanduseVectorModel.class );
            final ILanduseCoverageModel landuseCoverageModel = (ILanduseCoverageModel) scenarioDataProvider.getModel( ILanduseCoverageModel.class );
            final IFeatureWrapperCollection<ILandusePolygon> polygonCollection = landuseVectorModel.getLandusePolygonCollection();
            final IFeatureWrapperCollection<IWaterdepthCoverage> inputCoverages = waterdepthCoverageModel.getWaterdepthCoverageCollection();
            final ICoverageCollection outputCoverages = landuseCoverageModel.getCoverageCollection();

            for( final Iterator iterator = inputCoverages.iterator(); iterator.hasNext(); )
            {
              final IWaterdepthCoverage inputCoverage = (IWaterdepthCoverage) iterator.next();
              final IGeoGrid inputGrid = GeoGridUtilities.toGrid( inputCoverage );

              final IGeoGrid outputGrid = new AbstractDelegatingGeoGrid( inputGrid )
              {
                private RectifiedGridDomain m_gridDomain = inputCoverage.getGridDomain();

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
                    final GM_Position positionAt = m_gridDomain.getPositionAt( x, y );
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
              final String fileName = scenarioFolder.getFile( inputCoverage.getRangeSet().getFile().getFileName() ).getName();
              final String outputFilePath = "models/raster/output/landuse_rasterized_" + fileName;

              final IFile ifile = scenarioFolder.getFile( new Path( outputFilePath ) );
              final File file = new File( ifile.getRawLocation().toPortableString() );
              GeoGridUtilities.addCoverage( outputCoverages, outputGrid, file, outputFilePath, "image/bin", new NullProgressMonitor() );

              inputGrid.dispose();
              // ifile.refreshLocal( IFile.DEPTH_ZERO, new NullProgressMonitor() );

              // fireModellEvent to redraw a map...
              workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, waterdepthCoverageCollection.getWrappedFeature(), new Feature[] { inputCoverage.getWrappedFeature() }, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
            }

            scenarioDataProvider.postCommand( IWaterdepthCoverageModel.class, new EmptyCommand( "Get dirty!", false ) );
          }
          catch( final Exception e )
          {
            e.printStackTrace();
            throw new InterruptedException( e.getLocalizedMessage() );
          }
        }
      } );
    }
    catch( final Exception e )
    {
      ErrorDialog.openError( getShell(), Messages.getString( "ImportWaterdepthWizard.6" ), "", StatusUtilities.statusFromThrowable( e ) ); //$NON-NLS-1$ //$NON-NLS-2$
      e.printStackTrace();
      return false;
    }
    return true;
  }

}
