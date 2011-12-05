package org.kalypso.risk.model.utils;

import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.commons.i18n.I10nString;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.grid.AbstractDelegatingGeoGrid;
import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.kalypsosimulationmodel.utils.SLDHelper;
import org.kalypso.ogc.gml.AbstractCascadingLayerTheme;
import org.kalypso.ogc.gml.CascadingKalypsoTheme;
import org.kalypso.ogc.gml.CascadingThemeHelper;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IKalypsoCascadingTheme;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.handlers.MapHandlerUtils;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.actions.dataImport.waterdepth.AsciiRasterInfo;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.ILanduseClass;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IRiskLanduseStatistic;
import org.kalypso.risk.plugin.KalypsoRiskPlugin;
import org.kalypso.template.types.StyledLayerType;
import org.kalypso.template.types.StyledLayerType.Property;
import org.kalypso.template.types.StyledLayerType.Style;
import org.kalypso.transformation.transformer.GeoTransformerFactory;
import org.kalypso.transformation.transformer.IGeoTransformer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.xml.sax.SAXException;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author Thomas Jung
 */
public class RiskModelHelper
{
  /** themeId of the map-layer containing the events */
  public static final String THEME_ID__WATERDEPTH = "depthGridThemes"; //$NON-NLS-1$

  public static final String THEME_ID__DAMAGE_POTENTIAL = "damagePotentialThemes"; //$NON-NLS-1$

  public static final String THEME_NAME__WATERDEPTH = "%WaterlevelMap.mapv.gismapview.HQi"; //$NON-NLS-1$

  public static final String THEME_NAME__DAMAGE_POTENTIAL = "%SpecificDamagePotentialMap.gismapview.Schadenspotentiale"; //$NON-NLS-1$

  public static enum LAYER_TYPE
  {
    WATERLEVEL,
    SPECIFIC_DAMAGE_POTENTIAL
  }

  private static enum FIELD
  {
    STYLE_URN,
    THEMEINFO_CLASS,
    I18N_THEMEINFO_LABEL,
    I18N_LAYER_NAME
  }

  private static Map<LAYER_TYPE, Map<FIELD, String>> LAYER_PROPERTY_MAP = new HashMap<LAYER_TYPE, Map<FIELD, String>>()
  {
    {
      put( LAYER_TYPE.WATERLEVEL, new HashMap<FIELD, String>()
      {
        {
          put( FIELD.STYLE_URN, "urn:style:sld:risk:inundation:waterlevel" ); //$NON-NLS-1$
          put( FIELD.THEMEINFO_CLASS, "org.kalypso.gml.ui.map.CoverageThemeInfo" ); //$NON-NLS-1$
          put( FIELD.I18N_THEMEINFO_LABEL, "WaterlevelMap.gismapview.themeInfoLabel" ); //$NON-NLS-1$
          put( FIELD.I18N_LAYER_NAME, "WaterlevelMap.gismapview.layer" ); //$NON-NLS-1$
        }
      } );
      put( LAYER_TYPE.SPECIFIC_DAMAGE_POTENTIAL, new HashMap<FIELD, String>()
      {
        {
          put( FIELD.STYLE_URN, "urn:style:sld:risk:damage:specific" ); //$NON-NLS-1$
          put( FIELD.THEMEINFO_CLASS, "org.kalypso.risk.plugin.DamagePotentialThemeInfo" ); //$NON-NLS-1$
          put( FIELD.I18N_LAYER_NAME, "SpecificDamagePotentialMap.gismapview.layer" ); //$NON-NLS-1$
        }
      } );
    }
  };

  /**
   * updates the style for the specific annual damage value layers according to the overall min and max values.
   * 
   * @param scenarioFolder
   * @param model
   * @param sldFile
   * @throws IOException
   * @throws SAXException
   * @throws CoreException
   */
  public static void updateDamageStyle( final IFile sldFile, final IFeatureBindingCollection<IAnnualCoverageCollection> specificDamageCoverageCollection ) throws IOException, SAXException, CoreException
  {
    BigDecimal maxDamageValue = new BigDecimal( -Double.MAX_VALUE ).setScale( 4, BigDecimal.ROUND_HALF_UP );
    BigDecimal minDamageValue = new BigDecimal( Double.MAX_VALUE ).setScale( 4, BigDecimal.ROUND_HALF_UP );

    for( final ICoverageCollection specificDamageCoverage : specificDamageCoverageCollection )
    {
      try
      {
        final BigDecimal[] extrema = GeoGridUtilities.getMinMax( specificDamageCoverage );

        minDamageValue = minDamageValue.min( extrema[0] );
        maxDamageValue = maxDamageValue.max( extrema[1] );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }
    SLDHelper.exportRasterSymbolyzerSLD( sldFile, minDamageValue.doubleValue(), maxDamageValue.doubleValue() * 1.05, 20, Color.lightGray, Color.red, "Kalypso style", "Kalypso style", null ); //$NON-NLS-1$ //$NON-NLS-2$
    sldFile.refreshLocal( IResource.DEPTH_ZERO, new NullProgressMonitor() );
  }

  public static void fillStatistics( final int returnPeriod, final ILanduseClass landuseClass, final double damageValue, final double cellSize )
  {
    final IRiskLanduseStatistic statistic = RiskLanduseHelper.getLanduseStatisticEntry( landuseClass, returnPeriod, cellSize );
    final BigDecimal value = new BigDecimal( damageValue ).setScale( 2, BigDecimal.ROUND_HALF_UP );
    statistic.updateStatistic( value );
  }

  /**
   * Creates the specific damage coverage collection. <br>
   * The damage value for each grid cell is taken from the underlying polygon.
   * 
   * @param scenarioFolder
   *          scenario folder
   * @param polygonCollection
   *          landuse polygon collection
   * @param sourceCoverageCollection
   *          {@link CoverageCollection} with flow depth values
   * @param specificDamageCoverageCollection
   *          {@link CoverageCollection} with damage values
   * @return {@link CoverageCollection} with the annual damage values
   * @throws Exception
   */
  // TODO: nor more used, remove!?
  public static IAnnualCoverageCollection createSpecificDamageCoverages( final IFolder scenarioFolder, final IFeatureWrapperCollection<ILandusePolygon> polygonCollection, final IAnnualCoverageCollection sourceCoverageCollection, final IFeatureBindingCollection<IAnnualCoverageCollection> specificDamageCoverageCollection, final List<ILanduseClass> landuseClassesList ) throws Exception
  {
    final IAnnualCoverageCollection destCoverageCollection = specificDamageCoverageCollection.addNew( IAnnualCoverageCollection.QNAME );

    final int returnPeriod = sourceCoverageCollection.getReturnPeriod();

    final IFeatureBindingCollection<ICoverage> coverages = sourceCoverageCollection.getCoverages();
    for( int i = 0; i < coverages.size(); i++ )
    {
      final ICoverage inputCoverage = coverages.get( i );

      final IGeoGrid inputGrid = GeoGridUtilities.toGrid( inputCoverage );
      final double cellSize = Math.abs( inputGrid.getOffsetX().x - inputGrid.getOffsetY().x ) * Math.abs( inputGrid.getOffsetX().y - inputGrid.getOffsetY().y );

      final IGeoGrid outputGrid = new AbstractDelegatingGeoGrid( inputGrid )
      {
        /**
         * @see org.kalypso.grid.AbstractDelegatingGeoGrid#getValue(int, int) gets the damage value for each grid cell
         *      from the underlying polygon.
         */
        @Override
        public double getValue( final int x, final int y ) throws GeoGridException
        {
          try
          {
            final Double value = super.getValue( x, y );
            if( value.equals( Double.NaN ) )
              return Double.NaN;
            else
            {

              /* This coordinate has the cs of the input grid! */
              final Coordinate coordinate = GeoGridUtilities.toCoordinate( inputGrid, x, y, null );

              if( polygonCollection.size() == 0 )
                return Double.NaN;

              final ILandusePolygon landusePolygon = polygonCollection.get( 0 );
              final String coordinateSystem = landusePolygon.getGeometry().getCoordinateSystem();
              final GM_Position positionAt = JTSAdapter.wrap( coordinate );

              /* Transform query position into the cs of the polygons. */
              final IGeoTransformer geoTransformer = GeoTransformerFactory.getGeoTransformer( coordinateSystem );
              final GM_Position position = geoTransformer.transform( positionAt, inputGrid.getSourceCRS() );

              /* This list has some unknown cs. */

              final List<ILandusePolygon> list = polygonCollection.query( position );
              if( list == null || list.size() == 0 )
                return Double.NaN;
              else
              {
                for( final ILandusePolygon polygon : list )
                {
                  if( polygon.contains( position ) )
                  {
                    final int landuseClassOrdinalNumber = polygon.getLanduseClassOrdinalNumber();
                    final double damageValue = polygon.getDamageValue( value );

                    if( Double.isNaN( damageValue ) )
                      return Double.NaN;

                    if( damageValue < 0.0 )
                      return Double.NaN;

                    /* set statistic for landuse class */
                    fillStatistics( returnPeriod, landuseClassesList, damageValue, landuseClassOrdinalNumber, cellSize );
                    return damageValue;
                  }
                }
              }
              return Double.NaN;
            }
          }
          catch( final Exception ex )
          {
            throw new GeoGridException( Messages.getString( "org.kalypso.risk.model.utils.RiskModelHelper.0" ), ex ); //$NON-NLS-1$
          }
        }
      };

      /* add the new coverage to the collection */
      final String outputFilePath = "raster/output/specificDamage_HQ" + sourceCoverageCollection.getReturnPeriod() + "_part" + i + ".bin"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

      final IFile ifile = scenarioFolder.getFile( new Path( "models/" + outputFilePath ) ); //$NON-NLS-1$
      final File file = new File( ifile.getRawLocation().toPortableString() );

      final ICoverage newCoverage = GeoGridUtilities.addCoverage( destCoverageCollection, outputGrid, file, outputFilePath, "image/bin", new NullProgressMonitor() ); //$NON-NLS-1$

      for( final ILanduseClass landuseClass : landuseClassesList )
      {
        landuseClass.updateStatistic( returnPeriod );
      }
      newCoverage.setName( Messages.getString( "org.kalypso.risk.model.utils.RiskModelHelper.15", sourceCoverageCollection.getReturnPeriod(), i ) ); //$NON-NLS-1$
      newCoverage.setDescription( Messages.getString( "org.kalypso.risk.model.utils.RiskModelHelper.16", new Date().toString() ) ); //$NON-NLS-1$

      inputGrid.dispose();
    }
    /* set the return period of the specific damage grid */
    destCoverageCollection.setReturnPeriod( sourceCoverageCollection.getReturnPeriod() );
    return destCoverageCollection;
  }

  protected static void fillStatistics( final int returnPeriod, final List<ILanduseClass> landuseClassesList, final double damageValue, final int landuseClassOrdinalNumber, final double cellSize )
  {
    /* find the right landuse class that holds the polygon */// TODO: potential list search problem!
    for( final ILanduseClass landuseClass : landuseClassesList )
    {
      if( landuseClass.getOrdinalNumber() == landuseClassOrdinalNumber )
      {
        final IRiskLanduseStatistic statistic = RiskLanduseHelper.getLanduseStatisticEntry( landuseClass, returnPeriod, cellSize );

        final BigDecimal value = new BigDecimal( damageValue ).setScale( 2, BigDecimal.ROUND_HALF_UP );
        statistic.updateStatistic( value );
      }
    }
  }

  public static StyledLayerType createMapLayer( final LAYER_TYPE type, final IAnnualCoverageCollection coverageCollection ) throws Exception
  {
    final Map<FIELD, String> propertyMap = LAYER_PROPERTY_MAP.get( type );
    final String localizedLayerName = Messages.getString( propertyMap.get( FIELD.I18N_LAYER_NAME ) );
    final String layerName = String.format( localizedLayerName, coverageCollection.getReturnPeriod() );
    final String featurePath = String.format( "#fid#%s/coverageMember", coverageCollection.getId() ); //$NON-NLS-1$
    final String themeInfoClass = propertyMap.get( FIELD.THEMEINFO_CLASS );
    final String localizedThemeInfoLabel = propertyMap.containsKey( FIELD.I18N_THEMEINFO_LABEL ) ? Messages.getString( propertyMap.get( FIELD.I18N_THEMEINFO_LABEL ) ) : null;
    final String styleURN = propertyMap.get( FIELD.STYLE_URN );

    final StyledLayerType layer = new StyledLayerType();
    layer.setName( layerName );
    layer.setFeaturePath( featurePath );
    layer.setLinktype( "gml" ); //$NON-NLS-1$
    layer.setType( "simple" ); //$NON-NLS-1$
    layer.setVisible( true );
    layer.setActuate( "onRequest" ); //$NON-NLS-1$
    layer.setHref( "../models/RasterDataModel.gml" ); //$NON-NLS-1$
    layer.setVisible( true );
    final Property layerPropertyDeletable = new Property();
    layerPropertyDeletable.setName( IKalypsoTheme.PROPERTY_DELETEABLE );
    layerPropertyDeletable.setValue( "false" ); //$NON-NLS-1$
    final Property layerPropertyThemeInfoId = new Property();
    layerPropertyThemeInfoId.setName( IKalypsoTheme.PROPERTY_THEME_INFO_ID );
    if( localizedThemeInfoLabel == null )
      layerPropertyThemeInfoId.setValue( themeInfoClass );
    else
      layerPropertyThemeInfoId.setValue( String.format( "%s?format=%s", themeInfoClass, localizedThemeInfoLabel ) ); //$NON-NLS-1$
    final List<Property> layerPropertyList = layer.getProperty();
    layerPropertyList.add( layerPropertyDeletable );
    layerPropertyList.add( layerPropertyThemeInfoId );
    final List<Style> styleList = layer.getStyle();
    final Style style = new Style();
    style.setLinktype( "sld" ); //$NON-NLS-1$
    style.setStyle( "default" ); //$NON-NLS-1$
    style.setActuate( "onRequest" ); //$NON-NLS-1$
    style.setHref( styleURN );
    style.setType( "simple" ); //$NON-NLS-1$
    styleList.add( style );
    return layer;
  }

  public static StyledLayerType createSpecificDamageMapLayer( final IAnnualCoverageCollection coverageCollection ) throws Exception
  {
    return createMapLayer( LAYER_TYPE.SPECIFIC_DAMAGE_POTENTIAL, coverageCollection );
  }

  /**
   * creates a map layer for the grid collection
   * 
   * @param parentKalypsoTheme
   *          {@link AbstractCascadingLayerTheme} in which we add the new theme layer
   * @param coverageCollection
   *          {@link IAnnualCoverageCollection} that will be added
   * @param scenarioFolder
   * @throws Exception
   */
  public static void insertSpecificDamageMapLayer( final AbstractCascadingLayerTheme parentKalypsoTheme, final IAnnualCoverageCollection coverageCollection ) throws Exception
  {
    final StyledLayerType layer = createMapLayer( LAYER_TYPE.SPECIFIC_DAMAGE_POTENTIAL, coverageCollection );
    parentKalypsoTheme.addLayer( layer );
  }

  public static void deleteExistingMapLayers( final CascadingKalypsoTheme parentKalypsoTheme )
  {
    final IKalypsoTheme[] childThemes = parentKalypsoTheme.getAllThemes();
    for( final IKalypsoTheme childTheme : childThemes )
      parentKalypsoTheme.removeTheme( childTheme );
  }

  /**
   * calculates the average annual damage value for one raster cell <br>
   * further informations: DVWK-Mitteilung 10
   * 
   * @param damages
   *          damage values for all annualities
   * @param probabilities
   *          the probability values for all annualtities
   * @return damage potential value [€/a]
   */
  public static double calcAverageAnnualDamageValue( final double[] damages, final double[] probabilities )
  {
    /* support for single flood event (no annuality) */
    if( probabilities.length == 1 && damages.length > 0 )
      return damages[0];

    /* average annual damage value (jährlicher Schadenserwartungswert) [€/m²/a] */
    double result = 0.0;

    for( int j = 1; j < probabilities.length; j++ )
    {
      final double dP = Math.abs( probabilities[j - 1] - probabilities[j] );
      result += (damages[j - 1] + damages[j]) * dP / 2;
    }
    return result;
  }

  /**
   * 
   * creates the land use raster files. The grid cells get the ordinal number of the the land use class.
   * 
   * @param scenarioFolder
   *          relative path needed for the output file path to append on
   * @param inputCoverages
   *          {@link ICoverageCollection} that gives the extend of the raster
   * @param outputCoverages
   *          {@link ICoverageCollection} for the landuse
   * @param polygonCollection
   *          landuse polygons that give the landuse class ordinal number
   * @throws Exception
   */
  public static IStatus doRasterLanduse( final IFolder scenarioFolder, final ICoverageCollection inputCoverages, final ICoverageCollection outputCoverages, final IFeatureWrapperCollection<ILandusePolygon> polygonCollection, final IProgressMonitor monitor )
  {
    try
    {
      final IFeatureBindingCollection<ICoverage> coverages = inputCoverages.getCoverages();
      for( int i = 0; i < coverages.size(); i++ )
      {
        final ICoverage inputCoverage = coverages.get( i );
        final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString( "org.kalypso.risk.model.utils.RiskModelHelper.14", i + 1, coverages.size() ), 100 ); //$NON-NLS-1$

        final IGeoGrid inputGrid = GeoGridUtilities.toGrid( inputCoverage );
        final int sizeY = inputGrid.getSizeY();

        /* This grid should have the cs of the input grid. */
        final IGeoGrid outputGrid = new AbstractDelegatingGeoGrid( inputGrid )
        {
          /**
           * @see org.kalypso.grid.AbstractDelegatingGeoGrid#getValue(int, int) gets the ordinal number of the landuse
           *      class
           */
          @Override
          public double getValue( final int x, final int y ) throws GeoGridException
          {
            progress.setWorkRemaining( sizeY + 2 );

            try
            {
              final Double value = super.getValue( x, y );
              if( value.equals( Double.NaN ) )
                return Double.NaN;
              else
              {
                /* This coordinate has the cs of the input grid! */
                final Coordinate coordinate = GeoGridUtilities.toCoordinate( inputGrid, x, y, null );

                if( polygonCollection.size() == 0 )
                  return Double.NaN;

                final ILandusePolygon landusePolygon = polygonCollection.get( 0 );
                final String coordinateSystem = landusePolygon.getGeometry().getCoordinateSystem();
                final GM_Position positionAt = JTSAdapter.wrap( coordinate );

                /* Transform query position into the cs of the polygons. */
                final IGeoTransformer geoTransformer = GeoTransformerFactory.getGeoTransformer( coordinateSystem );
                final GM_Position position = geoTransformer.transform( positionAt, inputGrid.getSourceCRS() );

                /* This list has some unknown cs. */
                final List<ILandusePolygon> list = polygonCollection.query( position );
                if( list == null || list.size() == 0 )
                  return Double.NaN;
                else
                  for( final ILandusePolygon polygon : list )
                  {
                    if( polygon.contains( position ) )
                      return polygon.getLanduseClassOrdinalNumber();
                  }
                return Double.NaN;
              }
            }
            catch( final Exception ex )
            {
              throw new GeoGridException( Messages.getString( "org.kalypso.risk.model.utils.RiskModelHelper.10" ), ex ); //$NON-NLS-1$
            }
          }
        };

        // TODO: change name: better: use input name
        final String outputFilePath = "raster/output/LanduseCoverage" + i + ".dat"; //$NON-NLS-1$ //$NON-NLS-2$

        final IFile ifile = scenarioFolder.getFile( new Path( "models/" + outputFilePath ) ); //$NON-NLS-1$
        final File file = new File( ifile.getRawLocation().toPortableString() );

        GeoGridUtilities.addCoverage( outputCoverages, outputGrid, file, outputFilePath, "image/bin", new NullProgressMonitor() ); //$NON-NLS-1$
        inputGrid.dispose();
      }

      return Status.OK_STATUS;
    }
    catch( final Exception e )
    {
      return StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.risk.model.utils.RiskModelHelper.11" ) ); //$NON-NLS-1$
    }
  }

  /**
   * get the water depth raster with the greatest annuality
   * 
   * @param waterDepthCoverageCollection
   *          raster collection
   * @return {@link IAnnualCoverageCollection} with greatest return period value
   */
  public static IAnnualCoverageCollection getMaxReturnPeriodCollection( final IFeatureBindingCollection<IAnnualCoverageCollection> waterDepthCoverageCollection )
  {
    int maxReturnPeriod = Integer.MIN_VALUE;
    IAnnualCoverageCollection maxCoveragesCollection = null;
    for( final IAnnualCoverageCollection annualCoverageCollection : waterDepthCoverageCollection )
    {
      if( annualCoverageCollection.getReturnPeriod() > maxReturnPeriod && annualCoverageCollection.getCoverages().size() > 0 )
      {
        maxReturnPeriod = annualCoverageCollection.getReturnPeriod();
        maxCoveragesCollection = annualCoverageCollection;
      }
    }
    return maxCoveragesCollection;
  }

  /**
   * deletes the old layer, add the new one and modifies the style according to the max values
   * 
   * @param scenarioFolder
   * @param model
   * @param mapModell
   * @throws Exception
   * @throws IOException
   * @throws SAXException
   * @throws CoreException
   */
  public static void updateDamageLayers( final IFeatureBindingCollection<IAnnualCoverageCollection> specificDamageCoverageCollection, final GisTemplateMapModell mapModell ) throws Exception
  {
    /* get cascading them that holds the damage layers */

    final CascadingKalypsoTheme parentKalypsoTheme = CascadingThemeHelper.getNamedCascadingTheme( mapModell, THEME_NAME__DAMAGE_POTENTIAL, THEME_ID__DAMAGE_POTENTIAL ); //$NON-NLS-1$

    /* delete existing damage layers */
    deleteExistingMapLayers( parentKalypsoTheme );

    parentKalypsoTheme.setVisible( true );

    /* add the coverage collections to the map */
    for( final IAnnualCoverageCollection annualCoverageCollection : specificDamageCoverageCollection )
      insertSpecificDamageMapLayer( parentKalypsoTheme, annualCoverageCollection );
    parentKalypsoTheme.saveFeatures( new NullProgressMonitor() );
  }

  /**
   * deletes the old layers and adds the new ones
   * 
   * @param scenarioFolder
   * @param model
   * @param mapModell
   * @throws Exception
   * @throws IOException
   * @throws SAXException
   * @throws CoreException
   */
  public static void updateWaterdepthLayers( final IRasterDataModel model, final List<AsciiRasterInfo> rasterInfos, final GisTemplateMapModell mapModell ) throws Exception
  {
    /* get cascading them that holds the damage layers */
    final CascadingKalypsoTheme parentKalypsoTheme = CascadingThemeHelper.getNamedCascadingTheme( mapModell, THEME_NAME__WATERDEPTH, THEME_ID__WATERDEPTH ); //$NON-NLS-1$

    /* delete existing damage layers */
    // TODO: manage that only the newly imported gets deleted.
    deleteExistingMapLayers( parentKalypsoTheme, rasterInfos );

    parentKalypsoTheme.setVisible( true );

    final IFeatureBindingCollection<IAnnualCoverageCollection> waterdepthCoverageCollection = model.getWaterlevelCoverageCollection();

    for( int i = 0; i < rasterInfos.size(); i++ )
    {
      final AsciiRasterInfo asciiRasterInfo = rasterInfos.get( i );
      final int returnPeriod = asciiRasterInfo.getReturnPeriod();

      final int collectionIndex = getCollectionIndex( waterdepthCoverageCollection, returnPeriod );
      createWaterdepthLayer( parentKalypsoTheme, waterdepthCoverageCollection.get( collectionIndex ) );
    }
  }

  private static int getCollectionIndex( final IFeatureBindingCollection<IAnnualCoverageCollection> waterdepthCoverageCollection, final int returnPeriod )
  {
    int index = 0;

    for( int i = 0; i < waterdepthCoverageCollection.size(); i++ )
    {
      final IAnnualCoverageCollection collection = waterdepthCoverageCollection.get( i );
      if( collection.getReturnPeriod().equals( returnPeriod ) )
        index = i;
    }

    return index;
  }

  private static void deleteExistingMapLayers( final CascadingKalypsoTheme parentKalypsoTheme, final List<AsciiRasterInfo> rasterInfos )
  {
    final List<IKalypsoTheme> themesToRemove = new ArrayList<IKalypsoTheme>();

    for( int i = 0; i < rasterInfos.size(); i++ )
    {
      final AsciiRasterInfo asciiRasterInfo = rasterInfos.get( i );
      final String layerName = "HQ " + asciiRasterInfo.getReturnPeriod(); //$NON-NLS-1$
      final IKalypsoTheme[] childThemes = parentKalypsoTheme.getAllThemes();
      for( final IKalypsoTheme childTheme : childThemes )
        if( childTheme.getName().getKey().equals( layerName ) )
          themesToRemove.add( childTheme );
    }
    for( final IKalypsoTheme themeToRemove : themesToRemove )
      parentKalypsoTheme.removeTheme( themeToRemove );
  }

  private static void createWaterdepthLayer( final CascadingKalypsoTheme parentKalypsoTheme, final IAnnualCoverageCollection annualCoverageCollection ) throws Exception
  {
    final StyledLayerType layer = createMapLayer( LAYER_TYPE.WATERLEVEL, annualCoverageCollection );
    parentKalypsoTheme.addLayer( layer );
  }

  public static int guessReturnPeriodFromName( final String name )
  {
    final Pattern pattern = Pattern.compile( "([^0-9]*)([0-9]+)([^0-9]*)" ); //$NON-NLS-1$
    final Matcher matcher = pattern.matcher( name );
    if( matcher.matches() )
      return Integer.parseInt( matcher.group( 2 ) );
    return 0;
  }

  /**
   * calculates the average annual damage value for each landuse class<br>
   * The value is calculated by integrating the specific damage values.<br>
   * 
   */
  public static void calcLanduseAnnualAverageDamage( final IRasterizationControlModel rasterizationControlModel )
  {
    /* get the landuse classes */
    final List<ILanduseClass> landuseClassesList = rasterizationControlModel.getLanduseClassesList();
    if( landuseClassesList.size() == 0 )
      return;

    for( final ILanduseClass landuseClass : landuseClassesList )
    {
      /* get the statistical data for each landuse class */
      final List<IRiskLanduseStatistic> landuseStatisticList = landuseClass.getLanduseStatisticList();
      if( landuseStatisticList.size() == 0 )
        landuseClass.setAverageAnnualDamage( 0.0 );

      // generate a return period - sorted list
      final Map<Double, IRiskLanduseStatistic> periodSortedMap = new TreeMap<Double, IRiskLanduseStatistic>();
      for( int i = 0; i < landuseStatisticList.size(); i++ )
      {
        final IRiskLanduseStatistic riskLanduseStatistic = landuseStatisticList.get( i );
        final double period = riskLanduseStatistic.getReturnPeriod();
        periodSortedMap.put( period, riskLanduseStatistic );
      }

      final Set<Double> keySet = periodSortedMap.keySet();

      final Double[] periods = keySet.toArray( new Double[keySet.size()] );

      /* calculate the average annual damage by integrating the specific damage values */

      double averageSum = 0.0;

      for( int i = 0; i < periods.length - 1; i++ )
      {
        if( periods[i] == null || periods[i] == 0 )
          continue;

        /* get the probability for each return period */
        final double p1 = 1 / periods[i];
        final double p2 = 1 / periods[i + 1];

        /* calculate the difference */
        final double d_pi = p1 - p2;

        /*
         * get the specific damage summation value for this and the next return period an calculate the difference
         * (divided by 2). This means nothing else than to calculate the area for trapezoid with ha=specific value 1 and
         * hb= specific value 2. The width of the trapezoid is the difference of the probabilities that belong to both
         * specific damages values.
         */
        final IRiskLanduseStatistic statEntry1 = periodSortedMap.get( periods[i] );
        final IRiskLanduseStatistic statEntry2 = periodSortedMap.get( periods[i + 1] );

        // final BigDecimal sumStat = statEntry2.getDamageSum().add( statEntry1.getDamageSum() );
        final BigDecimal sumStat = statEntry2.getAverageDamage().add( statEntry1.getAverageDamage() );
        final double value = sumStat.doubleValue() / 2;
        final BigDecimal si = new BigDecimal( value ).setScale( 2, BigDecimal.ROUND_HALF_UP );

        /* calculate the average damage and add it */
        averageSum = averageSum + si.doubleValue() * d_pi;
      }

      /* set the average annual damage value for the current landuse class */
      landuseClass.setAverageAnnualDamage( averageSum );
    }
  }

  public static void addEventThemes( final IKalypsoCascadingTheme parentKalypsoTheme, final IAnnualCoverageCollection annualCoverageCollection ) throws CoreException
  {
    // Check, if theme already exists
    final String featurePath = "#fid#" + annualCoverageCollection.getId() + "/coverageMember"; //$NON-NLS-1$ //$NON-NLS-2$
    final IKalypsoFeatureTheme existingTheme = CascadingThemeHelper.findThemeWithFeaturePath( parentKalypsoTheme, featurePath );
    if( existingTheme != null )
    {
      existingTheme.setName( new I10nString( "HQ " + annualCoverageCollection.getReturnPeriod() ) ); //$NON-NLS-1$
      return;
    }

    final String layerName = annualCoverageCollection.getName();
    final StyledLayerType layer = new StyledLayerType();
    layer.setName( layerName );
    layer.setFeaturePath( featurePath );
    layer.setLinktype( "gml" ); //$NON-NLS-1$
    layer.setType( "simple" ); //$NON-NLS-1$
    layer.setVisible( true );
    layer.setActuate( "onRequest" ); //$NON-NLS-1$

    // please DO NOT externalize the paths (create constants if necessary)!
    layer.setHref( "../models/RasterDataModel.gml" ); //$NON-NLS-1$

    layer.setVisible( true );

    final Property layerPropertyDeletable = new Property();
    layerPropertyDeletable.setName( IKalypsoTheme.PROPERTY_DELETEABLE );
    layerPropertyDeletable.setValue( "false" ); //$NON-NLS-1$

    final Property layerPropertyThemeInfoId = new Property();
    layerPropertyThemeInfoId.setName( IKalypsoTheme.PROPERTY_THEME_INFO_ID );
    layerPropertyThemeInfoId.setValue( "org.kalypso.gml.ui.map.CoverageThemeInfo?format=Wassertiefe %.2f m" ); //$NON-NLS-1$

    final List<Property> layerPropertyList = layer.getProperty();
    layerPropertyList.add( layerPropertyDeletable );
    layerPropertyList.add( layerPropertyThemeInfoId );

    final List<Style> styleList = layer.getStyle();
    final Style style = new Style();
    style.setLinktype( "sld" ); //$NON-NLS-1$
    style.setStyle( "Kalypso style" ); //$NON-NLS-1$
    style.setActuate( "onRequest" ); //$NON-NLS-1$
    style.setHref( "../styles/WaterlevelCoverage.sld" ); //$NON-NLS-1$
    style.setType( "simple" ); //$NON-NLS-1$
    styleList.add( style );

    parentKalypsoTheme.addLayer( layer );
  }

  /**
   * Import new events into the risk model.<br>
   * 
   * The parameters 'names', 'returnPeriods', 'grids' must eb of the same size.
   * 
   * @param names
   *          The names of the events to import
   * @param descriptions
   *          The descriptions of the events to import
   * @param returnPeriods
   *          The return periods (probabilities) of the event to import.
   * @param grids
   *          Contains the coverages to import for every event.
   * */
  public static void importEvents( final String[] names, final String[] descriptions, final Integer[] returnPeriods, final ICoverageCollection[] grids, final IProgressMonitor monitor ) throws CoreException, Exception, InvocationTargetException
  {
    Assert.isTrue( names.length == grids.length );
    Assert.isTrue( names.length == returnPeriods.length );
    Assert.isTrue( names.length == descriptions.length );

    monitor.beginTask( Messages.getString( "org.kalypso.risk.model.utils.RiskModelHelper.1" ), names.length ); //$NON-NLS-1$

    /* The active scenario must have changed to the risk project. We can now access risk project data. */
    final SzenarioDataProvider riskDataProvider = ScenarioHelper.getScenarioDataProvider();

    final String failedToLoadRiskMsg = String.format( Messages.getString( "RiskModelHelper.0" ) ); //$NON-NLS-1$
    final IStatus failedToLoadRiskStatus = new Status( IStatus.ERROR, PluginUtilities.id( KalypsoRiskPlugin.getDefault() ), failedToLoadRiskMsg );
    if( !riskDataProvider.waitForModelToLoad( IRasterDataModel.class.getName(), 5 * 1000 ) )
      throw new CoreException( failedToLoadRiskStatus );

    final IRasterDataModel rasterDataModel = riskDataProvider.getModel( IRasterDataModel.class.getName(), IRasterDataModel.class );
    if( rasterDataModel == null )
      throw new CoreException( failedToLoadRiskStatus );

    final IFeatureBindingCollection<IAnnualCoverageCollection> waterlevelCoverageCollection = rasterDataModel.getWaterlevelCoverageCollection();

    /* --- demo code for accessing the depth grid coverage collections --- */
    final IContainer scenarioFolder = riskDataProvider.getScenarioFolder();
    final String rasterFolderPath = "raster/input/"; //$NON-NLS-1$
    final IFolder rasterFolder = (IFolder) scenarioFolder.findMember( "models/raster/input" ); //$NON-NLS-1$

    Assert.isNotNull( rasterFolder );

    /* Also add event themes to the map */
    // TRICKY: we get the map from the global map-context, let's hope it always works here
    final IHandlerService handlerService = (IHandlerService) PlatformUI.getWorkbench().getService( IHandlerService.class );
    final IEvaluationContext currentState = handlerService.getCurrentState();
    final IMapModell mapModell = MapHandlerUtils.getMapModell( currentState );
    final IKalypsoCascadingTheme wspThemes = mapModell != null ? getHQiTheme( mapModell ) : null;

    // get the result coverage collections (depth grids) from the events
    final List<Feature> createdFeatures = new ArrayList<Feature>();
    for( int i = 0; i < names.length; i++ )
    {
      final IAnnualCoverageCollection annualCoverageCollection = waterlevelCoverageCollection.addNew( IAnnualCoverageCollection.QNAME );
      annualCoverageCollection.setName( names[i] );
      annualCoverageCollection.setDescription( descriptions[i] );
      annualCoverageCollection.setReturnPeriod( returnPeriods[i] );
      createdFeatures.add( annualCoverageCollection );

      int coverageCount = 0;
      final IFeatureBindingCollection<ICoverage> coverages = grids[i].getCoverages();
      for( final ICoverage coverage : coverages )
      {
        final String subtaks = Messages.getString( "org.kalypso.risk.model.utils.RiskModelHelper.17", names[i], coverageCount + 1, coverages.size() ); //$NON-NLS-1$
        monitor.subTask( subtaks );

        // NO! When imported from Flood, return period is always 1 by default, so files will be overwritten!
        //        final String targetFileName = String.format( "grid_%d_%d", annualCoverageCollection.getReturnPeriod(), coverageCount ); //$NON-NLS-1$
        final String targetFileName = String.format( "grid_%s_%d", annualCoverageCollection.getId(), coverageCount ); //$NON-NLS-1$

        final IGeoGrid grid = GeoGridUtilities.toGrid( coverage );

        final File targetFile = FileUtilities.createNewUniqueFile( targetFileName, ".ascbin", rasterFolder.getLocation().toFile() ); //$NON-NLS-1$
        final String targetGridPath = rasterFolderPath + targetFile.getName();

        final SubMonitor subMonitor = SubMonitor.convert( monitor, 10 );
        final ICoverage newCoverage = GeoGridUtilities.addCoverage( annualCoverageCollection, grid, targetFile, targetGridPath, "image/bin", subMonitor ); //$NON-NLS-1$
        newCoverage.setName( Messages.getString( "org.kalypso.risk.model.utils.RiskModelHelper.18", coverageCount + 1 ) ); //$NON-NLS-1$
        newCoverage.setDescription( Messages.getString( "org.kalypso.risk.model.utils.RiskModelHelper.19", new Date() ) ); //$NON-NLS-1$
        grid.dispose();
        coverageCount++;
        if( !subMonitor.isCanceled() && wspThemes != null )
          addEventThemes( wspThemes, annualCoverageCollection );
      }

      ProgressUtilities.worked( monitor, 1 );
    }
    rasterFolder.refreshLocal( IFolder.DEPTH_INFINITE, monitor );

    /* ------ */
    // TODO: maybe save other models?
    final Feature f1 = (Feature) rasterDataModel.getFeature().getProperty( IRasterDataModel.PROPERTY_WATERLEVEL_COVERAGE_COLLECTION );
    final GMLWorkspace workspace = rasterDataModel.getFeature().getWorkspace();
    workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, f1, createdFeatures.toArray( new Feature[0] ), FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
    riskDataProvider.postCommand( IRasterDataModel.class.getName(), new EmptyCommand( Messages.getString( "org.kalypso.risk.model.utils.RiskModelHelper.20" ), false ) ); //$NON-NLS-1$
    riskDataProvider.saveModel( IRasterDataModel.class.getName(), new NullProgressMonitor() );
  }

  public static IKalypsoCascadingTheme getHQiTheme( final IMapModell mapModell )
  {
    // activate cascading theme that contains the events
    final AbstractCascadingLayerTheme byThemeId = CascadingThemeHelper.getCascadingThemeByProperty( mapModell, THEME_ID__WATERDEPTH );
    if( byThemeId != null )
      return byThemeId;

    return CascadingThemeHelper.getNamedCascadingTheme( mapModell, THEME_NAME__WATERDEPTH, THEME_ID__WATERDEPTH );
  }

  /**
   * Finds and activates the event theme if present.
   * 
   * @return <code>true</code>, if the theme was succesfully activated.
   * */
  public static boolean activateEventTheme( final IMapPanel mapPanel )
  {
    final IMapModell mapModell = mapPanel.getMapModell();
    if( mapModell == null )
      return false;

    final IKalypsoTheme hqiTheme = getHQiTheme( mapModell );
    if( hqiTheme == null )
      return false;

    mapModell.activateTheme( hqiTheme );

    return true;
  }

}
