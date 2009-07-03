package org.kalypso.risk.model.utils;

import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.grid.AbstractDelegatingGeoGrid;
import org.kalypso.grid.ConvertAscii2Binary;
import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.GeoGridUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.kalypsosimulationmodel.utils.SLDHelper;
import org.kalypso.ogc.gml.AbstractCascadingLayerTheme;
import org.kalypso.ogc.gml.CascadingKalypsoTheme;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.risk.model.actions.dataImport.waterdepth.AsciiRasterInfo;
import org.kalypso.risk.model.actions.dataImport.waterdepth.Messages;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.template.types.StyledLayerType;
import org.kalypso.template.types.StyledLayerType.Property;
import org.kalypso.template.types.StyledLayerType.Style;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
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
  /**
   * gets the {@link CascadingKalypsoTheme} with the given name
   * 
   * @param mapModell
   *            map modell
   * @param name
   *            name of the theme
   * @return
   */
  public static CascadingKalypsoTheme getCascadingTheme( final GisTemplateMapModell mapModell, final String name )
  {
    final IKalypsoTheme[] allThemes = mapModell.getAllThemes();
    for( final IKalypsoTheme kalypsoTheme : allThemes )
    {
      if( kalypsoTheme instanceof CascadingKalypsoTheme && kalypsoTheme.getName().equals( name ) ) //$NON-NLS-1$
        return (CascadingKalypsoTheme) kalypsoTheme;
    }
    return null;
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
  public static void updateDamageStyle( final IFile sldFile, final IRasterDataModel model ) throws IOException, SAXException, CoreException
  {
    /* update style */
    final IFeatureWrapperCollection<IAnnualCoverageCollection> specificDamageCoverageCollections = model.getSpecificDamageCoverageCollection();

    BigDecimal maxDamageValue = new BigDecimal( Double.MIN_VALUE ).setScale( 4, BigDecimal.ROUND_HALF_UP );
    BigDecimal minDamageValue = new BigDecimal( Double.MAX_VALUE ).setScale( 4, BigDecimal.ROUND_HALF_UP );

    for( ICoverageCollection collection : specificDamageCoverageCollections )
    {
      try
      {
        final BigDecimal[] extrema = GeoGridUtilities.getMinMax( collection );

        minDamageValue = minDamageValue.min( extrema[0] );
        maxDamageValue = maxDamageValue.max( extrema[1] );
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }

    SLDHelper.exportRasterSymbolyzerSLD( sldFile, minDamageValue.doubleValue(), maxDamageValue.doubleValue() * 1.05, 20, new Color( 237, 80, 25 ), "Kalypso style", "Kalypso style", null ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * Creates the specific damage coverage collection. <br>
   * The damage value for each grid cell is taken from the underlying polygon.
   * 
   * @param scenarioFolder
   *            scenario folder
   * @param polygonCollection
   *            landuse polygon collection
   * @param sourceCoverageCollection
   *            {@link CoverageCollection} with flow depth values
   * @param specificDamageCoverageCollection
   *            {@link CoverageCollection} with damage values
   * @return {@link CoverageCollection} with the annual damage values
   * @throws Exception
   */
  public static IAnnualCoverageCollection createSpecificDamageCoverages( final IFolder scenarioFolder, final IFeatureWrapperCollection<ILandusePolygon> polygonCollection, final IAnnualCoverageCollection sourceCoverageCollection, final IFeatureWrapperCollection<IAnnualCoverageCollection> specificDamageCoverageCollection ) throws Exception
  {
    final IAnnualCoverageCollection destCoverageCollection = specificDamageCoverageCollection.addNew( IAnnualCoverageCollection.QNAME );

    for( int i = 0; i < sourceCoverageCollection.size(); i++ )
    {
      final ICoverage inputCoverage = sourceCoverageCollection.get( i );

      final IGeoGrid inputGrid = GeoGridUtilities.toGrid( inputCoverage );

      final IGeoGrid outputGrid = new AbstractDelegatingGeoGrid( inputGrid )
      {
        /**
         * @see org.kalypso.grid.AbstractDelegatingGeoGrid#getValue(int, int)
         * 
         * gets the damage value for each grid cell from the underlying polygon.
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
            {
              for( final ILandusePolygon polygon : list )
              {
                if( polygon.contains( positionAt ) )
                {
                  final double damageValue = polygon.getDamageValue( value );

                  if( Double.isNaN( damageValue ) )
                    return Double.NaN;

                  if( damageValue < 0.0 )
                    return Double.NaN;

                  return damageValue;
                }
              }
            }
            return Double.NaN;
          }
        }
      };

      /* add the new coverage to the collection */
      final String outputFilePath = "raster/output/specificDamage_HQ" + sourceCoverageCollection.getReturnPeriod() + "_part" + i + ".bin";

      final IFile ifile = scenarioFolder.getFile( new Path( "models/" + outputFilePath ) );
      final File file = new File( ifile.getRawLocation().toPortableString() );

      final ICoverage newCoverage = GeoGridUtilities.addCoverage( destCoverageCollection, outputGrid, file, outputFilePath, "image/bin", new NullProgressMonitor() );

      newCoverage.setName( Messages.getString( "DamagePotentialCalculationHandler.14" ) + sourceCoverageCollection.getReturnPeriod() + " [" + i + "]" );
      // TODO: check for right time zone?
      newCoverage.setDescription( Messages.getString( "DamagePotentialCalculationHandler.17" ) + new Date().toString() );

      inputGrid.dispose();
    }
    /* set the return period of the specific damage grid */
    destCoverageCollection.setReturnPeriod( sourceCoverageCollection.getReturnPeriod() );
    return destCoverageCollection;
  }

  /**
   * creates a map layer for the grid collection
   * 
   * @param parentKalypsoTheme
   *            {@link AbstractCascadingLayerTheme} in which we add the new theme layer
   * @param coverageCollection
   *            {@link IAnnualCoverageCollection} that will be added
   * @param scenarioFolder
   * @throws Exception
   */
  public static void createDamagePotentialMapLayer( final AbstractCascadingLayerTheme parentKalypsoTheme, final IAnnualCoverageCollection coverageCollection, final IResource scenarioFolder ) throws Exception
  {
    final String layerName = Messages.getString( "DamagePotentialCalculationHandler.13" ) + coverageCollection.getReturnPeriod() + ")"; //$NON-NLS-1$ //$NON-NLS-2$

    // TODO: this is dangerous!
    // it is better to delete all damage layers, because what happens if the user changes the annuality and
    // re-calculates.
    // remove themes that are showing invalid coverages
    // final IKalypsoTheme[] childThemes = parentKalypsoTheme.getAllThemes();
    // final List<IKalypsoTheme> themesToRemove = new ArrayList<IKalypsoTheme>();
    // for( int i = 0; i < childThemes.length; i++ )
    // the existing layer with the previous annuality gets not deleted.
    // if( childThemes[i].getName().equals( layerName ) )
    // themesToRemove.add( childThemes[i] );
    // for( final IKalypsoTheme themeToRemove : themesToRemove )
    // parentKalypsoTheme.removeTheme( themeToRemove );

    final StyledLayerType layer = new StyledLayerType();
    layer.setName( layerName );
    layer.setFeaturePath( "#fid#" + coverageCollection.getFeature().getId() + "/coverageMember" ); //$NON-NLS-1$ //$NON-NLS-2$
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
  }

  public static void deleteExistingMapLayers( CascadingKalypsoTheme parentKalypsoTheme )
  {
    final IKalypsoTheme[] childThemes = parentKalypsoTheme.getAllThemes();
    for( int i = 0; i < childThemes.length; i++ )
      parentKalypsoTheme.removeTheme( childThemes[i] );
  }

  /**
   * calculates the average annual damage value for one raster cell <br>
   * further informations: DVWK-Mitteilung 10
   * 
   * @param damages
   *            damage values for all annualities
   * @param probabilities
   *            the probability values for all annualtities
   * @return damage potential value [€/a]
   */
  public static double calcAverageAnnualDamageValue( final double[] damages, final double[] probabilities )
  {
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
   * creates the land use raster files. The grid cells get the ordinal number of the the land use class.
   * 
   * @param scenarioFolder
   *            relative path needed for the output file path to append on
   * @param inputCoverages
   *            {@link ICoverageCollection} that gives the extend of the raster
   * @param outputCoverages
   *            {@link ICoverageCollection} for the landuse
   * @param polygonCollection
   *            landuse polygons that give the landuse class ordinal number
   * @throws Exception
   */
  public static IStatus doRasterLanduse( final IFolder scenarioFolder, final ICoverageCollection inputCoverages, final ICoverageCollection outputCoverages, final IFeatureWrapperCollection<ILandusePolygon> polygonCollection )
  {
    try
    {
      for( int i = 0; i < inputCoverages.size(); i++ )
      {
        final ICoverage inputCoverage = inputCoverages.get( i );

        final IGeoGrid inputGrid = GeoGridUtilities.toGrid( inputCoverage );

        final IGeoGrid outputGrid = new AbstractDelegatingGeoGrid( inputGrid )
        {
          /**
           * @see org.kalypso.grid.AbstractDelegatingGeoGrid#getValue(int, int)
           * 
           * gets the ordinal number of the landuse class
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
        final String outputFilePath = "raster/output/LanduseCoverage" + i + ".dat"; //$NON-NLS-1$ //$NON-NLS-2$

        final IFile ifile = scenarioFolder.getFile( new Path( "models/" + outputFilePath ) ); //$NON-NLS-1$
        final File file = new File( ifile.getRawLocation().toPortableString() );
        GeoGridUtilities.addCoverage( outputCoverages, outputGrid, file, outputFilePath, "image/bin", new NullProgressMonitor() ); //$NON-NLS-1$
        inputGrid.dispose();
      }

      return Status.OK_STATUS;
    }
    catch( Exception e )
    {
      return StatusUtilities.statusFromThrowable( e, "Fehler bei Erzeugung des Ländnutzungsrasters." );
    }
  }

  /**
   * get the water depth raster with the greatest annuality
   * 
   * @param waterDepthCoverageCollection
   *            raster collection
   * @return {@link IAnnualCoverageCollection} with greatest return period value
   */
  public static IAnnualCoverageCollection getMaxReturnPeriodCollection( IFeatureWrapperCollection<IAnnualCoverageCollection> waterDepthCoverageCollection )
  {
    int maxReturnPeriod = Integer.MIN_VALUE;
    IAnnualCoverageCollection maxCoveragesCollection = null;
    for( final IAnnualCoverageCollection annualCoverageCollection : waterDepthCoverageCollection )
    {
      if( annualCoverageCollection.getReturnPeriod() > maxReturnPeriod && annualCoverageCollection.size() > 0 )
      {
        maxReturnPeriod = annualCoverageCollection.getReturnPeriod();
        maxCoveragesCollection = annualCoverageCollection;
      }
    }
    return maxCoveragesCollection;
  }

  public static IStatus importAsBinaryRaster( final File srcFile, final File dstFile, final IProgressMonitor monitor ) throws IOException, CoreException, GeoGridException
  {
    final ConvertAscii2Binary ascii2Binary = new ConvertAscii2Binary( srcFile.toURL(), dstFile, 2 );
    ascii2Binary.doConvert( monitor );
    return Status.OK_STATUS;
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
  public static void updateDamageLayers( final IFolder scenarioFolder, final IRasterDataModel model, final GisTemplateMapModell mapModell ) throws Exception
  {
    /* get cascading them that holds the damage layers */
    final CascadingKalypsoTheme parentKalypsoTheme = getCascadingTheme( mapModell, "Schadenspotentiale" );

    /* delete existing damage layers */
    deleteExistingMapLayers( parentKalypsoTheme );

    parentKalypsoTheme.setVisible( true );

    /* add the coverage collections to the map */
    IFeatureWrapperCollection<IAnnualCoverageCollection> specificDamageCoverageCollection = model.getSpecificDamageCoverageCollection();
    for( IAnnualCoverageCollection annualCoverageCollection : specificDamageCoverageCollection )
      createDamagePotentialMapLayer( parentKalypsoTheme, annualCoverageCollection, scenarioFolder );

    final IFile sldFile = scenarioFolder.getFile( "/styles/SpecificDamagePotentialCoverage.sld" ); //$NON-NLS-1$
    updateDamageStyle( sldFile, model );
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
  public static void updateWaterdepthLayers( final IFolder scenarioFolder, final IRasterDataModel model, final List<AsciiRasterInfo> rasterInfos, final GisTemplateMapModell mapModell ) throws Exception
  {
    /* get cascading them that holds the damage layers */
    final CascadingKalypsoTheme parentKalypsoTheme = getCascadingTheme( mapModell, "HQi" );

    /* delete existing damage layers */
    // TODO: manage that only the newly imported gets deleted.
    deleteExistingMapLayers( parentKalypsoTheme, rasterInfos );

    parentKalypsoTheme.setVisible( true );

    IFeatureWrapperCollection<IAnnualCoverageCollection> waterdepthCoverageCollection = model.getWaterlevelCoverageCollection();

    for( int i = 0; i < rasterInfos.size(); i++ )
    {
      final AsciiRasterInfo asciiRasterInfo = rasterInfos.get( i );
      final String layerName = "HQ " + asciiRasterInfo.getReturnPeriod(); //$NON-NLS-1$

      createWaterdepthLayer( scenarioFolder, parentKalypsoTheme, waterdepthCoverageCollection.get( i ), layerName );
    }
  }

  private static void deleteExistingMapLayers( CascadingKalypsoTheme parentKalypsoTheme, List<AsciiRasterInfo> rasterInfos )
  {
    final List<IKalypsoTheme> themesToRemove = new ArrayList<IKalypsoTheme>();

    for( int i = 0; i < rasterInfos.size(); i++ )
    {
      final AsciiRasterInfo asciiRasterInfo = rasterInfos.get( i );
      final String layerName = "HQ " + asciiRasterInfo.getReturnPeriod(); //$NON-NLS-1$
      final IKalypsoTheme[] childThemes = parentKalypsoTheme.getAllThemes();
      for( int j = 0; j < childThemes.length; j++ )
        if( childThemes[j].getName().equals( layerName ) )
          themesToRemove.add( childThemes[j] );
    }
    for( final IKalypsoTheme themeToRemove : themesToRemove )
      parentKalypsoTheme.removeTheme( themeToRemove );
  }

  private static void createWaterdepthLayer( final IFolder scenarioFolder, final CascadingKalypsoTheme parentKalypsoTheme, final IAnnualCoverageCollection annualCoverageCollection, final String layerName ) throws Exception
  {
    final StyledLayerType layer = new StyledLayerType();
    layer.setName( layerName );
    layer.setFeaturePath( "#fid#" + annualCoverageCollection.getGmlID() + "/coverageMember" ); //$NON-NLS-1$ //$NON-NLS-2$
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

  public static int guessReturnPeriodFromName( final String name )
  {
    final Pattern pattern = Pattern.compile( "([^0-9]*)([0-9]+)([^0-9]*)" ); //$NON-NLS-1$
    final Matcher matcher = pattern.matcher( name );
    if( matcher.matches() )
      return Integer.parseInt( matcher.group( 2 ) );
    return 0;
  }
}
