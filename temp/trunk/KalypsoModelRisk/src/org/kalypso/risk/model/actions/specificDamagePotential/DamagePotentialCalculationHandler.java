package org.kalypso.risk.model.actions.specificDamagePotential;

import java.awt.Color;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

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
import org.kalypso.grid.AbstractDelegatingGeoGrid;
import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.kalypsosimulationmodel.utils.SLDHelper;
import org.kalypso.ogc.gml.CascadingKalypsoTheme;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.risk.model.actions.dataImport.waterdepth.Messages;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IVectorDataModel;
import org.kalypso.template.types.StyledLayerType;
import org.kalypso.template.types.StyledLayerType.Property;
import org.kalypso.template.types.StyledLayerType.Style;
import org.kalypso.ui.views.map.MapView;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

public class DamagePotentialCalculationHandler extends AbstractHandler
{
  private double m_minDamageValue = Double.MAX_VALUE;

  private double m_maxDamageValue = Double.MIN_VALUE;

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
    final Dialog dialog = new MessageDialog( shell, "Rasterizing specific annual damage", null, "Do you want to calcualte specific annual damage?", MessageDialog.QUESTION, new String[] { "Ja", "Nein" }, 0 );
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
          MessageDialog.openError( shell, "Error", "No HQ data is loaded. Damage potential cannot be calculated. Please load waterlevel raster data." );
          return null;
        }
        final IVectorDataModel vectorDataModel = (IVectorDataModel) scenarioDataProvider.getModel( IVectorDataModel.class );
        final IRasterizationControlModel rasterizationControlModel = scenarioDataProvider.getModel( IRasterizationControlModel.class );
        if( rasterizationControlModel.getAssetValueClassesList().size() == 0 )
        {
          MessageDialog.openError( shell, "Error", "No asset value classes are properly defined. Damage potential cannot be calculated. Please define asset value classes." );
          return null;
        }
        final IFeatureWrapperCollection<ILandusePolygon> polygonCollection = vectorDataModel.getLandusePolygonCollection();
        final GMLWorkspace workspace = scenarioDataProvider.getCommandableWorkSpace( IRasterDataModel.class );
        final GisTemplateMapModell mapModell = (GisTemplateMapModell) mapView.getMapPanel().getMapModell();
        final CascadingKalypsoTheme parentKalypsoTheme = getCascadingTheme( mapModell );
        parentKalypsoTheme.setVisible( true );
        new ProgressMonitorDialog( shell ).run( true, false, new IRunnableWithProgress()
        {
          public void run( final IProgressMonitor monitor ) throws InterruptedException
          {
            monitor.beginTask( "Rasterizing specific annual damage...", IProgressMonitor.UNKNOWN );
            try
            {
              for( final IAnnualCoverageCollection srcAnnualCoverages : model.getWaterlevelCoverageCollection() )
              {
                monitor.subTask( "Calculating specific annual damage for HQ " + srcAnnualCoverages.getReturnPeriod() );

                final IFeatureWrapperCollection<IAnnualCoverageCollection> specificDamageCoverageCollection = model.getSpecificDamageCoverageCollection();
                // TODO: check if still ok: propbably delete all underlying grids
                
                // remove existing (invalid) coverages from the model
                final List<IAnnualCoverageCollection> coveragesToRemove = new ArrayList<IAnnualCoverageCollection>();
                for( final IAnnualCoverageCollection existingAnnualCoverage : specificDamageCoverageCollection )
                  if( existingAnnualCoverage.getReturnPeriod() == srcAnnualCoverages.getReturnPeriod() )
                    coveragesToRemove.add( existingAnnualCoverage );
                for( final IAnnualCoverageCollection coverageToRemove : coveragesToRemove )
                  specificDamageCoverageCollection.remove( coverageToRemove );

                final IAnnualCoverageCollection dstAnnualCoverages = specificDamageCoverageCollection.addNew( IAnnualCoverageCollection.QNAME );

                int count = 0;
                for( final ICoverage inputCoverage : srcAnnualCoverages )
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
                            {
                              final double damageValue = polygon.getDamageValue( value );
                              if( m_minDamageValue > damageValue )
                                m_minDamageValue = damageValue;
                              if( m_maxDamageValue < damageValue )
                                m_maxDamageValue = damageValue;
                              return damageValue;
                            }
                          }
                        return Double.NaN;
                      }
                    }
                  };
                  
                  // TODO: change count to better name...
                  final String outputFilePath = "raster/output/specificDamage_HQ" + srcAnnualCoverages.getReturnPeriod()+"_part" + count + ".bin";

                  final IFile ifile = scenarioFolder.getFile( new Path( "models/" + outputFilePath ) );
                  final File file = new File( ifile.getRawLocation().toPortableString() );
                  
                  GeoGridUtilities.addCoverage( dstAnnualCoverages, outputGrid, file, outputFilePath, "image/bin", new NullProgressMonitor() );

                  inputGrid.dispose();
                  
                  count++;
                }

                dstAnnualCoverages.setReturnPeriod( srcAnnualCoverages.getReturnPeriod() );

                // create map layer
                monitor.subTask( Messages.getString( "ImportWaterdepthWizard.10" ) ); //$NON-NLS-1$
                final String layerName = "Schadenspotential (HQ " + dstAnnualCoverages.getReturnPeriod() + ")";
                // remove themes that are showing invalid coverages
                final IKalypsoTheme[] childThemes = parentKalypsoTheme.getAllThemes();
                final List<IKalypsoTheme> themesToRemove = new ArrayList<IKalypsoTheme>();
                for( int i = 0; i < childThemes.length; i++ )
                  if( childThemes[i].getName().equals( layerName ) )
                    themesToRemove.add( childThemes[i] );
                for( final IKalypsoTheme themeToRemove : themesToRemove )
                  parentKalypsoTheme.removeTheme( themeToRemove );

                final StyledLayerType layer = new StyledLayerType();
                layer.setName( layerName );
                layer.setFeaturePath( "#fid#" + dstAnnualCoverages.getWrappedFeature().getId()  + "/coverageMember");
                layer.setLinktype( "gml" );
                layer.setType( "simple" );
                layer.setVisible( true );
                layer.setActuate( "onRequest" );
                layer.setHref( "project:/" + scenarioFolder.getProjectRelativePath() + "/models/RasterDataModel.gml" );
                layer.setVisible( true );
                final Property layerPropertyDeletable = new Property();
                layerPropertyDeletable.setName( IKalypsoTheme.PROPERTY_DELETEABLE );
                layerPropertyDeletable.setValue( "false" );
                final Property layerPropertyThemeInfoId = new Property();
                layerPropertyThemeInfoId.setName( IKalypsoTheme.PROPERTY_THEME_INFO_ID );
                layerPropertyThemeInfoId.setValue( "org.kalypso.gml.ui.map.CoverageThemeInfo?format=Schadenspotential %.2f �/m�" );
                final List<Property> layerPropertyList = layer.getProperty();
                layerPropertyList.add( layerPropertyDeletable );
                layerPropertyList.add( layerPropertyThemeInfoId );
                final List<Style> styleList = layer.getStyle();
                final Style style = new Style();
                style.setLinktype( "sld" );
                style.setStyle( "Kalypso style" );
                style.setActuate( "onRequest" );
                style.setHref( "../styles/SpecificDamagePotentialCoverage.sld" );
                style.setType( "simple" );
                styleList.add( style );

                parentKalypsoTheme.addLayer( layer );

                // fireModellEvent to redraw a map...
                workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, model.getSpecificDamageCoverageCollection().getWrappedFeature(), new Feature[] { dstAnnualCoverages.getWrappedFeature() }, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
              }

              scenarioDataProvider.postCommand( IRasterDataModel.class, new EmptyCommand( "Get dirty!", false ) );
            }
            catch( final Exception e )
            {
              e.printStackTrace();
              throw new InterruptedException( e.getLocalizedMessage() );
            }
          }
        } );

        final IFile sldFile = scenarioFolder.getFile( "/styles/SpecificDamagePotentialCoverage.sld" );
        SLDHelper.exportRasterSymbolyzerSLD( sldFile, m_minDamageValue, m_maxDamageValue * 1.05, 20, new Color( 237, 80, 25 ), "Kalypso style", "Kalypso style", null );

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

  private CascadingKalypsoTheme getCascadingTheme( final GisTemplateMapModell mapModell )
  {
    final IKalypsoTheme[] allThemes = mapModell.getAllThemes();
    for( final IKalypsoTheme kalypsoTheme : allThemes )
    {
      if( kalypsoTheme instanceof CascadingKalypsoTheme && kalypsoTheme.getName().equals( "SpecificDamagePotential" ) )
        return (CascadingKalypsoTheme) kalypsoTheme;
    }
    return null;
  }
}
