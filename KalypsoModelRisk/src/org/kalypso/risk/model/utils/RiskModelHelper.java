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
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
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
import org.kalypso.ogc.gml.CascadingThemeHelper;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IKalypsoCascadingTheme;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoSaveableTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.handlers.MapHandlerUtils;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.actions.dataImport.waterdepth.AsciiRasterInfo;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.plugin.KalypsoRiskPlugin;
import org.kalypso.template.types.StyledLayerType;
import org.kalypso.template.types.StyledLayerType.Property;
import org.kalypso.template.types.StyledLayerType.Style;
import org.kalypso.transformation.transformer.GeoTransformerFactory;
import org.kalypso.transformation.transformer.IGeoTransformer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.xml.sax.SAXException;

import com.vividsolutions.jts.geom.Coordinate;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

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

  /**
   * Please use fine scale for the risk calculation, as many landuse types have very low damage values in EUR per m2. If
   * it is rounded to one cent (scale 2) before updating the statistics, the total statistics will be wrong at the end.
   */
  public static int BIGDECIMAL_SCALE_FINE = 16;

  public static int BIGDECIMAL_SCALE_MEDIUM = 4;

  public static int BIGDECIMAL_SCALE_COARSE = 2;

  public static enum LAYER_TYPE
  {
    WATERLEVEL,
    SPECIFIC_DAMAGE_POTENTIAL
  }

  private static enum FIELD
  {
    STYLE_URN,
    STYLE_NAME,
    THEMEINFO_CLASS,
    I18N_THEMEINFO_LABEL,
    I18N_LAYER_NAME
  }

  private static Map<LAYER_TYPE, Map<FIELD, String>> LAYER_PROPERTY_MAP = new HashMap<>();

  static
  {
    LAYER_PROPERTY_MAP.put( LAYER_TYPE.WATERLEVEL, new HashMap<FIELD, String>()
    {
      {
        put( FIELD.STYLE_URN, "../styles/WaterlevelCoverage.sld" ); //$NON-NLS-1$
        put( FIELD.THEMEINFO_CLASS, "org.kalypso.gml.ui.map.CoverageThemeInfo" ); //$NON-NLS-1$
        put( FIELD.I18N_THEMEINFO_LABEL, "WaterlevelMap.gismapview.themeInfoLabel" ); //$NON-NLS-1$
        put( FIELD.I18N_LAYER_NAME, "WaterlevelMap.gismapview.layer" ); //$NON-NLS-1$
      }
    } );
    LAYER_PROPERTY_MAP.put( LAYER_TYPE.SPECIFIC_DAMAGE_POTENTIAL, new HashMap<FIELD, String>()
    {
      {
        put( FIELD.STYLE_URN, "urn:style:sld:risk:damage:specific" ); //$NON-NLS-1$
        put( FIELD.STYLE_NAME, "default" ); //$NON-NLS-1$
        put( FIELD.THEMEINFO_CLASS, "org.kalypso.risk.plugin.DamagePotentialThemeInfo" ); //$NON-NLS-1$
        put( FIELD.I18N_LAYER_NAME, "SpecificDamagePotentialMap.gismapview.layer" ); //$NON-NLS-1$
      }
    } );
  }

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
    BigDecimal maxDamageValue = new BigDecimal( -Double.MAX_VALUE ).setScale( RiskModelHelper.BIGDECIMAL_SCALE_MEDIUM, BigDecimal.ROUND_HALF_UP );
    BigDecimal minDamageValue = new BigDecimal( Double.MAX_VALUE ).setScale( RiskModelHelper.BIGDECIMAL_SCALE_MEDIUM, BigDecimal.ROUND_HALF_UP );

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

  public static StyledLayerType createMapLayer( final LAYER_TYPE type, final IAnnualCoverageCollection coverageCollection ) throws Exception
  {
    final Map<FIELD, String> propertyMap = LAYER_PROPERTY_MAP.get( type );
    final String localizedLayerName = Messages.getString( propertyMap.get( FIELD.I18N_LAYER_NAME ) );
    final String layerName = String.format( localizedLayerName, coverageCollection.getReturnPeriod() );
    final String featurePath = String.format( "#fid#%s/coverageMember", coverageCollection.getId() ); //$NON-NLS-1$
    final String themeInfoClass = propertyMap.get( FIELD.THEMEINFO_CLASS );
    final String localizedThemeInfoLabel = propertyMap.containsKey( FIELD.I18N_THEMEINFO_LABEL ) ? Messages.getString( propertyMap.get( FIELD.I18N_THEMEINFO_LABEL ) ) : null;
    final String styleURN = propertyMap.get( FIELD.STYLE_URN );
    final String styleName = propertyMap.get( FIELD.STYLE_NAME );

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
    style.setStyle( styleName );
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
  public static void insertSpecificDamageMapLayer( final IKalypsoCascadingTheme parentKalypsoTheme, final IAnnualCoverageCollection coverageCollection ) throws Exception
  {
    final StyledLayerType layer = createMapLayer( LAYER_TYPE.SPECIFIC_DAMAGE_POTENTIAL, coverageCollection );
    parentKalypsoTheme.addLayer( layer );
  }

  public static void deleteExistingMapLayers( final IKalypsoCascadingTheme parentKalypsoTheme )
  {
    final IKalypsoTheme[] childThemes = parentKalypsoTheme.getAllThemes();
    for( final IKalypsoTheme childTheme : childThemes )
      parentKalypsoTheme.removeTheme( childTheme );
  }

  /**
   * Calculates the annual damage value from a list of specific damages and probabilites by integration.<br>
   * further informations: DVWK-Mitteilung 10
   * 
   * @param damages
   *          damage values for all annualities
   * @param probabilities
   *          the probability values for all annualtities
   * @return damage potential value [€/a]
   */
  public static double calcPotentialAnnualDamageValue( final double[] damages, final double[] probabilities )
  {
    /* support for single flood event (no annuality) */
    if( probabilities.length == 1 && damages.length > 0 )
      return damages[0];

    /* average annual damage value (jährlicher Schadenserwartungswert) [€/m²/a] */
    double result = 0.0;

    for( int j = 1; j < probabilities.length; j++ )
    {
      final double dP = Math.abs( probabilities[j - 1] - probabilities[j] );
      final double meanHeight = (damages[j - 1] + damages[j]) / 2;
      result += meanHeight * dP;
    }
    return result;
  }

  /**
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
  public static IStatus doRasterLanduse( final IFolder scenarioFolder, final ICoverageCollection inputCoverages, final ICoverageCollection outputCoverages, final IFeatureBindingCollection<ILandusePolygon> polygonCollection, final IProgressMonitor monitor )
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
    final IKalypsoCascadingTheme parentKalypsoTheme = CascadingThemeHelper.getNamedCascadingTheme( mapModell, THEME_NAME__DAMAGE_POTENTIAL, THEME_ID__DAMAGE_POTENTIAL ); //$NON-NLS-1$

    /* delete existing damage layers */
    deleteExistingMapLayers( parentKalypsoTheme );

    parentKalypsoTheme.setVisible( true );

    /* add the coverage collections to the map */
    for( final IAnnualCoverageCollection annualCoverageCollection : specificDamageCoverageCollection )
      insertSpecificDamageMapLayer( parentKalypsoTheme, annualCoverageCollection );

    if( parentKalypsoTheme instanceof IKalypsoSaveableTheme )
      ((IKalypsoSaveableTheme)parentKalypsoTheme).saveFeatures( new NullProgressMonitor() );
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
    final IKalypsoCascadingTheme parentKalypsoTheme = CascadingThemeHelper.getNamedCascadingTheme( mapModell, THEME_NAME__WATERDEPTH, THEME_ID__WATERDEPTH ); //$NON-NLS-1$

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

  private static void deleteExistingMapLayers( final IKalypsoCascadingTheme parentKalypsoTheme, final List<AsciiRasterInfo> rasterInfos )
  {
    final List<IKalypsoTheme> themesToRemove = new ArrayList<>();

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

  private static void createWaterdepthLayer( final IKalypsoCascadingTheme parentKalypsoTheme, final IAnnualCoverageCollection annualCoverageCollection ) throws Exception
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
    style.setActuate( "onRequest" ); //$NON-NLS-1$
    style.setHref( "../styles/WaterlevelCoverage.sld" ); //$NON-NLS-1$
    style.setType( "simple" ); //$NON-NLS-1$
    styleList.add( style );

    parentKalypsoTheme.addLayer( layer );
  }

  /**
   * Import new events into the risk model.<br>
   * The parameters 'names', 'returnPeriods', 'grids' must be of the same size.
   * 
   * @param names
   *          The names of the events to import
   * @param descriptions
   *          The descriptions of the events to import
   * @param returnPeriods
   *          The return periods (probabilities) of the event to import.
   * @param grids
   *          Contains the coverages to import for every event.
   */
  public static void importEvents( final String[] names, final String[] descriptions, final Integer[] returnPeriods, final ICoverageCollection[] grids, final IProgressMonitor monitor ) throws CoreException, Exception, InvocationTargetException
  {
    Assert.isTrue( names.length == grids.length );
    Assert.isTrue( names.length == returnPeriods.length );
    Assert.isTrue( names.length == descriptions.length );

    monitor.beginTask( Messages.getString( "org.kalypso.risk.model.utils.RiskModelHelper.1" ), names.length ); //$NON-NLS-1$

    /* The active scenario must have changed to the risk project. We can now access risk project data. */
    final IScenarioDataProvider riskDataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();

    final String failedToLoadRiskMsg = String.format( Messages.getString( "RiskModelHelper.0" ) ); //$NON-NLS-1$
    final IStatus failedToLoadRiskStatus = new Status( IStatus.ERROR, PluginUtilities.id( KalypsoRiskPlugin.getDefault() ), failedToLoadRiskMsg );
    if( !riskDataProvider.waitForModelToLoad( IRasterDataModel.class.getName(), 5 * 1000 ) )
      throw new CoreException( failedToLoadRiskStatus );

    final IRasterDataModel rasterDataModel = riskDataProvider.getModel( IRasterDataModel.class.getName() );
    if( rasterDataModel == null )
      throw new CoreException( failedToLoadRiskStatus );

    final IFeatureBindingCollection<IAnnualCoverageCollection> waterlevelCoverageCollection = rasterDataModel.getWaterlevelCoverageCollection();

    /* --- demo code for accessing the depth grid coverage collections --- */
    final IContainer scenarioFolder = riskDataProvider.getScenarioFolder();
    final String rasterFolderPath = "raster/input/"; //$NON-NLS-1$
    final IFolder rasterFolder = (IFolder)scenarioFolder.findMember( "models/raster/input" ); //$NON-NLS-1$

    Assert.isNotNull( rasterFolder );

    /* Also add event themes to the map */
    // TRICKY: we get the map from the global map-context, let's hope it always works here
    final IHandlerService handlerService = (IHandlerService)PlatformUI.getWorkbench().getService( IHandlerService.class );
    final IEvaluationContext currentState = handlerService.getCurrentState();
    final IMapModell mapModell = MapHandlerUtils.getMapModell( currentState );
    final IKalypsoCascadingTheme wspThemes = mapModell != null ? getHQiTheme( mapModell ) : null;

    // get the result coverage collections (depth grids) from the events
    final List<Feature> createdFeatures = new ArrayList<>();
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
    final Feature f1 = (Feature)rasterDataModel.getProperty( IRasterDataModel.PROPERTY_WATERLEVEL_COVERAGE_COLLECTION );
    final GMLWorkspace workspace = rasterDataModel.getWorkspace();
    workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, f1, createdFeatures.toArray( new Feature[0] ), FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
    riskDataProvider.postCommand( IRasterDataModel.class.getName(), new EmptyCommand( Messages.getString( "org.kalypso.risk.model.utils.RiskModelHelper.20" ), false ) ); //$NON-NLS-1$
    riskDataProvider.saveModel( IRasterDataModel.class.getName(), new NullProgressMonitor() );
  }

  public static IKalypsoCascadingTheme getHQiTheme( final IMapModell mapModell )
  {
    // activate cascading theme that contains the events
    final IKalypsoCascadingTheme byThemeId = CascadingThemeHelper.getCascadingThemeByProperty( mapModell, THEME_ID__WATERDEPTH );
    if( byThemeId != null )
      return byThemeId;

    return CascadingThemeHelper.getNamedCascadingTheme( mapModell, THEME_NAME__WATERDEPTH, THEME_ID__WATERDEPTH );
  }

  /**
   * Finds and activates the event theme if present.
   * 
   * @return <code>true</code>, if the theme was successfully activated.
   */
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