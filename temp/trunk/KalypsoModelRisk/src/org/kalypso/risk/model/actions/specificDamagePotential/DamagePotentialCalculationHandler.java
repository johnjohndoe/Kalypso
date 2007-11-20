package org.kalypso.risk.model.actions.specificDamagePotential;

import java.awt.Color;
import java.io.File;
import java.util.ArrayList;
import java.util.Date;
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
        final GisTemplateMapModell mapModell = (GisTemplateMapModell) mapView.getMapPanel().getMapModell();
        final CascadingKalypsoTheme parentKalypsoTheme = getCascadingTheme( mapModell );
        parentKalypsoTheme.setVisible( true );
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
                              if( Double.isNaN( damageValue ) )
                                return Double.NaN;
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
                  final String outputFilePath = "raster/output/specificDamage_HQ" + srcAnnualCoverages.getReturnPeriod() + "_part" + count + ".bin"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

                  final IFile ifile = scenarioFolder.getFile( new Path( "models/" + outputFilePath ) ); //$NON-NLS-1$
                  final File file = new File( ifile.getRawLocation().toPortableString() );

                  final ICoverage newCoverage = GeoGridUtilities.addCoverage( dstAnnualCoverages, outputGrid, file, outputFilePath, "image/bin", new NullProgressMonitor() ); //$NON-NLS-1$
                  newCoverage.setName( Messages.getString( "DamagePotentialCalculationHandler.14" ) + srcAnnualCoverages.getReturnPeriod() + " [" + count + "]" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                  newCoverage.setDescription( Messages.getString( "DamagePotentialCalculationHandler.17" ) + new Date().toString() ); //$NON-NLS-1$

                  inputGrid.dispose();
                  count++;
                }

                dstAnnualCoverages.setReturnPeriod( srcAnnualCoverages.getReturnPeriod() );

                // create map layer
                monitor.subTask( "" ); //$NON-NLS-1$
                final String layerName = Messages.getString( "DamagePotentialCalculationHandler.13" ) + dstAnnualCoverages.getReturnPeriod() + ")"; //$NON-NLS-1$ //$NON-NLS-2$
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
                layer.setFeaturePath( "#fid#" + dstAnnualCoverages.getWrappedFeature().getId() + "/coverageMember" ); //$NON-NLS-1$ //$NON-NLS-2$
                layer.setLinktype( "gml" ); //$NON-NLS-1$
                layer.setType( "simple" ); //$NON-NLS-1$
                layer.setVisible( true );
                layer.setActuate( "onRequest" ); //$NON-NLS-1$
                layer.setHref( "project:/" + scenarioFolder.getProjectRelativePath() + "/models/RasterDataModel.gml" ); //$NON-NLS-1$ //$NON-NLS-2$
                layer.setVisible( true );
                final Property layerPropertyDeletable = new Property();
                layerPropertyDeletable.setName( IKalypsoTheme.PROPERTY_DELETEABLE );
                layerPropertyDeletable.setValue( "false" ); //$NON-NLS-1$
                final Property layerPropertyThemeInfoId = new Property();
                layerPropertyThemeInfoId.setName( IKalypsoTheme.PROPERTY_THEME_INFO_ID );
                layerPropertyThemeInfoId.setValue( "org.kalypso.gml.ui.map.CoverageThemeInfo?format=Schadenspotential %.2f €/m²" ); //$NON-NLS-1$
                final List<Property> layerPropertyList = layer.getProperty();
                layerPropertyList.add( layerPropertyDeletable );
                layerPropertyList.add( layerPropertyThemeInfoId );
                final List<Style> styleList = layer.getStyle();
                final Style style = new Style();
                style.setLinktype( "sld" ); //$NON-NLS-1$
                style.setStyle( "Kalypso style" ); //$NON-NLS-1$
                style.setActuate( "onRequest" ); //$NON-NLS-1$
                style.setHref( "../styles/SpecificDamagePotentialCoverage.sld" ); //$NON-NLS-1$
                style.setType( "simple" ); //$NON-NLS-1$
                styleList.add( style );

                parentKalypsoTheme.addLayer( layer );

                // fireModellEvent to redraw a map...
                workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, model.getSpecificDamageCoverageCollection().getWrappedFeature(), new Feature[] { dstAnnualCoverages.getWrappedFeature() }, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
              }

              scenarioDataProvider.postCommand( IRasterDataModel.class, new EmptyCommand( "Get dirty!", false ) ); //$NON-NLS-1$

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

        final IFile sldFile = scenarioFolder.getFile( "/styles/SpecificDamagePotentialCoverage.sld" ); //$NON-NLS-1$
        SLDHelper.exportRasterSymbolyzerSLD( sldFile, m_minDamageValue, m_maxDamageValue * 1.05, 20, new Color( 237, 80, 25 ), "Kalypso style", "Kalypso style", null ); //$NON-NLS-1$ //$NON-NLS-2$

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
      if( kalypsoTheme instanceof CascadingKalypsoTheme && kalypsoTheme.getName().equals( "Schadenspotentials" ) ) //$NON-NLS-1$
        return (CascadingKalypsoTheme) kalypsoTheme;
    }
    return null;
  }
}
