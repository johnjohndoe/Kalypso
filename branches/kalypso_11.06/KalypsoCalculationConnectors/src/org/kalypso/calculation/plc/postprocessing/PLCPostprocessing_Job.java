package org.kalypso.calculation.plc.postprocessing;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import ogc31.www.opengis.net.gml.FileType;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.runtime.Path;
import org.kalypso.calculation.UrlCatalog;
import org.kalypso.model.flood.binding.IFloodModel;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.simulation.core.AbstractInternalStatusJob;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridCoverage;

public class PLCPostprocessing_Job extends AbstractInternalStatusJob implements ISimulation
{
  public static final String ID = "KalypsoPLC_Postprocessing"; //$NON-NLS-1$

  // flood inputs
  public static final String INPUT_FLOOD_DIFFERENCE_MODEL = "floodDifferenceModel";

  public static final String INPUT_FLOOD_DIFFERENCE_RESULT_FOLDER = "floodDifferenceResultFolder";

  public static final String INPUT_FLOOD_CALCULATED_MODEL = "floodCalculatedModel";

  public static final String INPUT_FLOOD_CALCULATED_RESULT_FOLDER = "floodCalculatedResultFolder";

  public static final String INPUT_FLOOD_STATUS_QUO_MODEL = "floodStatusQuoModel";

  public static final String INPUT_FLOOD_STATUS_QUO_RESULT_FOLDER = "floodStatusQuoResultFolder";

  // na inputs
  public static final String INPUT_NA_RESULTS_FOLDER = "naResultsFolder"; //$NON-NLS-1$

  // risk inputs
  public static final String INPUT_RISK_RASTERIZATION_CONTROL_MODEL = "riskRasterizationControlModel";

  public static final String INPUT_RISK_DIFFERENCE_RASTER_DATA_MODEL = "riskDifferenceRasterDataModel";

  public static final String INPUT_RISK_CALCULATED_RASTER_DATA_MODEL = "riskCalculatedRasterDataModel";

  public static final String INPUT_RISK_DIFFERENCE_RASTER_FOLDER_OUTPUT = "riskDifferenceRasterFolderOutput";

  public static final String INPUT_RISK_CALCULATED_RASTER_FOLDER_OUTPUT = "riskCalculatedRasterFolderOutput";

  public static final String INPUT_RISK_STATUS_QUO_RASTER_FOLDER_OUTPUT = "riskStatusQuoRasterFolderOutput";

  public static final String INPUT_RISK_DIFFERENCE_STATISTICS = "riskDifferenceStatistics";

  public static final String INPUT_RISK_STATUS_QUO_RASTER_DATA_MODEL = "riskStatusQuoRasterDataModel";

  // output folder
  public static final String OUTPUT_FOLDER = "outputFolder";

  // private
  private static final String EVENTS_FOLDER_NAME = "events";

  private static final QName INUNDATION_STATUSQUO_COVERAGECOLLECTION = new QName( UrlCatalog.NS_CCHAINRESULTS, "inundationStatusQuoCoverageCollection" ); //$NON-NLS-1$

  private static final QName INUNDATION_CALCULATED_COVERAGECOLLECTION = new QName( UrlCatalog.NS_CCHAINRESULTS, "inundationCalculatedCoverageCollection" ); //$NON-NLS-1$

  private static final QName INUNDATION_DIFFERENCE_COVERAGECOLLECTION = new QName( UrlCatalog.NS_CCHAINRESULTS, "inundationDifferenceCoverageCollection" ); //$NON-NLS-1$

  private static final QName RISK_STATUSQUO_COVERAGECOLLECTION = new QName( UrlCatalog.NS_CCHAINRESULTS, "riskStatusQuoCoverageCollection" ); //$NON-NLS-1$

  private static final QName RISK_CALCULATED_COVERAGECOLLECTION = new QName( UrlCatalog.NS_CCHAINRESULTS, "riskCalculatedCoverageCollection" ); //$NON-NLS-1$

  private static final QName RISK_DIFFERENCE_COVERAGECOLLECTION = new QName( UrlCatalog.NS_CCHAINRESULTS, "riskDifferenceCoverageCollection" ); //$NON-NLS-1$

  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( "resources/modelSpecification.xml" ); //$NON-NLS-1$
  }

  public URL getTemplate( )
  {
    return getClass().getResource( "resources/template.gml" ); //$NON-NLS-1$
  }

  public URL getSLD( )
  {
    return getClass().getResource( "resources/RiskZonesCoverage.sld" ); //$NON-NLS-1$
  }

  @Override
  public void run( final File outputFolder, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    try
    {
      new File( outputFolder, "doNotDelete.txt" ).createNewFile(); //$NON-NLS-1$

      final GMLWorkspace resultsWorkspace = GmlSerializer.createGMLWorkspace( getTemplate(), null );
      addInundationResults( resultsWorkspace, inputProvider, outputFolder );
      addRiskResults( resultsWorkspace, inputProvider, outputFolder );
      final File file = new File( outputFolder, "result.gml" ); //$NON-NLS-1$
      GmlSerializer.serializeWorkspace( file, resultsWorkspace, "UTF-8" );

      addNAResults( inputProvider, outputFolder );
    }
    catch( final Exception e )
    {
      Logger.getAnonymousLogger().log( Level.SEVERE, e.getLocalizedMessage() );
      throw new SimulationException( "Problem bei der Verarbeitung der Ergebnisse der Prozesskette", e );
    }
    resultEater.addResult( OUTPUT_FOLDER, outputFolder ); //$NON-NLS-1$
    setStatus( STATUS.OK, "Success" );
  }

  private void addNAResults( final ISimulationDataProvider inputProvider, final File outputFolder ) throws SimulationException, IOException
  {
    if( !inputProvider.hasID( INPUT_NA_RESULTS_FOLDER ) )
      return;

    final File naResultsFolder = FileUtils.toFile( (URL) inputProvider.getInputForID( INPUT_NA_RESULTS_FOLDER ) );
    final File naFolder = new File( outputFolder, "rrm" ); //$NON-NLS-1$
    naFolder.mkdirs();
    FileUtils.moveDirectoryToDirectory( naResultsFolder, naFolder, false );
  }

  private void addRiskResults( final GMLWorkspace resultsWorkspace, final ISimulationDataProvider inputProvider, final File outputFolder ) throws SimulationException, Exception, IOException, GmlSerializeException
  {
    if( !inputProvider.hasID( INPUT_RISK_STATUS_QUO_RASTER_DATA_MODEL ) )
      return;

    // adding risk zones coverages to the model
    final GMLWorkspace riskStatusQuoRasterDataModelWS = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( INPUT_RISK_STATUS_QUO_RASTER_DATA_MODEL ), null );
    final IRasterDataModel riskStatusQuoRasterDataModel = (IRasterDataModel) riskStatusQuoRasterDataModelWS.getRootFeature().getAdapter( IRasterDataModel.class );
    final File riskStatusQuoRasterFolderOutput = FileUtils.toFile( (URL) inputProvider.getInputForID( INPUT_RISK_STATUS_QUO_RASTER_FOLDER_OUTPUT ) );
    final ICoverageCollection riskStatusQuoCoverageCollection = (ICoverageCollection) ((Feature) resultsWorkspace.getRootFeature().getProperty( RISK_STATUSQUO_COVERAGECOLLECTION )).getAdapter( ICoverageCollection.class );
    for( final ICoverage coverage : riskStatusQuoRasterDataModel.getRiskZonesCoverage().getCoverages() )
    {
      changeCoverageFilePathPrefix( coverage, "statusQuo/" + riskStatusQuoRasterFolderOutput.getName() ); //$NON-NLS-1$
      riskStatusQuoCoverageCollection.getCoverages().add( coverage );
    }

    final GMLWorkspace riskCalculatedRasterDataModelWS = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( INPUT_RISK_CALCULATED_RASTER_DATA_MODEL ), null );
    final IRasterDataModel riskCalculatedRasterDataModel = (IRasterDataModel) riskCalculatedRasterDataModelWS.getRootFeature().getAdapter( IRasterDataModel.class );
    final ICoverageCollection riskCalculatedCoverageCollection = (ICoverageCollection) ((Feature) resultsWorkspace.getRootFeature().getProperty( RISK_CALCULATED_COVERAGECOLLECTION )).getAdapter( ICoverageCollection.class );
    final File riskCalculatedRasterFolderOutput = FileUtils.toFile( (URL) inputProvider.getInputForID( INPUT_RISK_CALCULATED_RASTER_FOLDER_OUTPUT ) );
    for( final ICoverage coverage : riskCalculatedRasterDataModel.getRiskZonesCoverage().getCoverages() )
    {
      changeCoverageFilePathPrefix( coverage, "calculated/" + riskCalculatedRasterFolderOutput.getName() ); //$NON-NLS-1$
      riskCalculatedCoverageCollection.getCoverages().add( coverage );
    }

    final GMLWorkspace riskDifferenceRasterDataModelWS = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( INPUT_RISK_DIFFERENCE_RASTER_DATA_MODEL ), null );
    final IRasterDataModel riskDifferenceRasterDataModel = (IRasterDataModel) riskDifferenceRasterDataModelWS.getRootFeature().getAdapter( IRasterDataModel.class );
    final ICoverageCollection riskDifferenceCoverageCollection = (ICoverageCollection) ((Feature) resultsWorkspace.getRootFeature().getProperty( RISK_DIFFERENCE_COVERAGECOLLECTION )).getAdapter( ICoverageCollection.class );
    final File riskDifferenceRasterFolderOutput = FileUtils.toFile( (URL) inputProvider.getInputForID( INPUT_RISK_DIFFERENCE_RASTER_FOLDER_OUTPUT ) );
    for( final ICoverage coverage : riskDifferenceRasterDataModel.getRiskZonesCoverage().getCoverages() )
    {
      changeCoverageFilePathPrefix( coverage, "difference/" + riskDifferenceRasterFolderOutput.getName() ); //$NON-NLS-1$
      riskDifferenceCoverageCollection.getCoverages().add( coverage );
    }

    final File riskFolder = new File( outputFolder, "risk" ); //$NON-NLS-1$
    riskFolder.mkdir();
    final File file = new File( riskFolder, "result.gml" ); //$NON-NLS-1$
    GmlSerializer.serializeWorkspace( file, resultsWorkspace, "UTF-8" );

    final File controlModelFile = new File( riskFolder, "RasterizationControlModel.gml" ); //$NON-NLS-1$
    final File sldFile = new File( riskFolder, "RiskZonesCoverage.sld" ); //$NON-NLS-1$
    try
    {
      FileUtils.copyURLToFile( (URL) inputProvider.getInputForID( INPUT_RISK_RASTERIZATION_CONTROL_MODEL ), controlModelFile ); //$NON-NLS-1$
      FileUtils.copyURLToFile( getSLD(), sldFile );
      // FileUtils.copyFileToDirectory( sldFile, riskFolder );
    }
    catch( final Exception e )
    {
      Logger.getAnonymousLogger().log( Level.WARNING, "Could not copy SLD file. Reason: " + e.getLocalizedMessage() );
    }

    final File statisticsFile = FileUtils.toFile( (URL) inputProvider.getInputForID( INPUT_RISK_DIFFERENCE_STATISTICS ) );
    FileUtils.moveFileToDirectory( statisticsFile, riskFolder, true );

    final File riskFolderStatusQuo = new File( riskFolder, "statusQuo" ); //$NON-NLS-1$
    FileUtils.moveDirectoryToDirectory( riskStatusQuoRasterFolderOutput, riskFolderStatusQuo, true );

    final File riskFolderCalculated = new File( riskFolder, "calculated" ); //$NON-NLS-1$
    FileUtils.moveDirectoryToDirectory( riskCalculatedRasterFolderOutput, riskFolderCalculated, true );

    final File riskFolderDifference = new File( riskFolder, "difference" ); //$NON-NLS-1$
    FileUtils.moveDirectoryToDirectory( riskDifferenceRasterFolderOutput, riskFolderDifference, true );
  }

  private void addInundationResults( final GMLWorkspace resultsWorkspace, final ISimulationDataProvider inputProvider, final File outputFolder ) throws SimulationException, Exception, IOException, GmlSerializeException
  {
    if( !inputProvider.hasID( INPUT_FLOOD_STATUS_QUO_MODEL ) )
      return;

    final File floodFolder = new File( outputFolder, "flood" ); //$NON-NLS-1$

    // STATUS QUO
    final File floodFolderStatusQuo = new File( floodFolder, "statusQuo" ); //$NON-NLS-1$
    floodFolderStatusQuo.mkdirs();
    final File floodStatusQuoRasterFolderInput = FileUtils.toFile( (URL) inputProvider.getInputForID( INPUT_FLOOD_STATUS_QUO_RESULT_FOLDER ) ); //$NON-NLS-1$
    FileUtils.moveDirectoryToDirectory( floodStatusQuoRasterFolderInput, floodFolderStatusQuo, false );

    final GMLWorkspace floodStatusQuoWS = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( INPUT_FLOOD_STATUS_QUO_MODEL ), null ); //$NON-NLS-1$
    final IFloodModel floodStatusQuoModel = (IFloodModel) floodStatusQuoWS.getRootFeature().getAdapter( IFloodModel.class );
    final IFeatureWrapperCollection<IRunoffEvent> statusQuoEvents = floodStatusQuoModel.getEvents();

    // adding representative (highest HQ) inundation coverages to the model
    int highestReturnPeriod = 1;
    for( final IRunoffEvent coverageCollection : statusQuoEvents )
    {
      final int returnPeriod = coverageCollection.getReturnPeriod();
      if( returnPeriod > highestReturnPeriod )
      {
        highestReturnPeriod = returnPeriod;
      }
    }

    final ICoverageCollection inundationStatusQuoCoverageCollection = (ICoverageCollection) ((Feature) resultsWorkspace.getRootFeature().getProperty( INUNDATION_STATUSQUO_COVERAGECOLLECTION )).getAdapter( ICoverageCollection.class );
    final String statusQuoPath = "flood/statusQuo/events"; //$NON-NLS-1$
    for( final IRunoffEvent coverageCollection : statusQuoEvents )
    {
      if( coverageCollection.getReturnPeriod() == highestReturnPeriod )
      {
        final IFeatureBindingCollection<ICoverage> coverages = coverageCollection.getResultCoverages().getCoverages();
        for( final ICoverage coverage : coverages )
        {
          changeCoverageFilePathPrefix( coverage, statusQuoPath );
          inundationStatusQuoCoverageCollection.getCoverages().add( coverage );
        }
      }
    }

    // CALCULATED
    final File floodFolderCalculated = new File( floodFolder, "calculated" ); //$NON-NLS-1$
    floodFolderCalculated.mkdirs();
    final File floodCalculatedRasterFolderInput = FileUtils.toFile( (URL) inputProvider.getInputForID( INPUT_FLOOD_CALCULATED_RESULT_FOLDER ) ); //$NON-NLS-1$
    FileUtils.moveDirectoryToDirectory( floodCalculatedRasterFolderInput, floodFolderCalculated, false );

    final GMLWorkspace floodCalculatedWS = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( INPUT_FLOOD_CALCULATED_MODEL ), null ); //$NON-NLS-1$
    final IFloodModel floodCalculatedModel = (IFloodModel) floodCalculatedWS.getRootFeature().getAdapter( IFloodModel.class );
    final ICoverageCollection inundationCalculatedCoverageCollection = (ICoverageCollection) ((Feature) resultsWorkspace.getRootFeature().getProperty( INUNDATION_CALCULATED_COVERAGECOLLECTION )).getAdapter( ICoverageCollection.class );
    final String calculatedPath = "flood/calculated/events"; //$NON-NLS-1$
    final IFeatureWrapperCollection<IRunoffEvent> calculatedEvents = floodCalculatedModel.getEvents();
    for( final IRunoffEvent coverageCollection : calculatedEvents )
    {
      if( coverageCollection.getReturnPeriod() == highestReturnPeriod )
      {
        final IFeatureBindingCollection<ICoverage> coverages = coverageCollection.getResultCoverages().getCoverages();
        for( final ICoverage coverage : coverages )
        {
          changeCoverageFilePathPrefix( coverage, calculatedPath ); //$NON-NLS-1$
          inundationCalculatedCoverageCollection.getCoverages().add( coverage );
        }
      }
    }

    // DIFFERENCE
    final File floodFolderDifference = new File( floodFolder, "difference" ); //$NON-NLS-1$
    floodFolderDifference.mkdirs();
    final File floodDifferenceRasterFolderInput = FileUtils.toFile( (URL) inputProvider.getInputForID( INPUT_FLOOD_DIFFERENCE_RESULT_FOLDER ) ); //$NON-NLS-1$
    FileUtils.moveDirectoryToDirectory( floodDifferenceRasterFolderInput, floodFolderDifference, false );

    final GMLWorkspace floodDifferenceWS = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( INPUT_FLOOD_DIFFERENCE_MODEL ), null ); //$NON-NLS-1$
    final IFloodModel floodDifferenceModel = (IFloodModel) floodDifferenceWS.getRootFeature().getAdapter( IFloodModel.class );
    final ICoverageCollection inundationDifferenceCoverageCollection = (ICoverageCollection) ((Feature) resultsWorkspace.getRootFeature().getProperty( INUNDATION_DIFFERENCE_COVERAGECOLLECTION )).getAdapter( ICoverageCollection.class );
    final String differencePath = "flood/difference/events"; //$NON-NLS-1$
    final IFeatureWrapperCollection<IRunoffEvent> differenceEvents = floodDifferenceModel.getEvents();
    for( final IRunoffEvent coverageCollection : differenceEvents )
    {
      if( coverageCollection.getReturnPeriod() == highestReturnPeriod )
      {
        final IFeatureBindingCollection<ICoverage> coverages = coverageCollection.getResultCoverages().getCoverages();
        for( final ICoverage coverage : coverages )
        {
          changeCoverageFilePathPrefix( coverage, differencePath ); //$NON-NLS-1$
          inundationDifferenceCoverageCollection.getCoverages().add( coverage );
        }
      }
    }
  }

  private final void changeCoverageFilePathPrefix( final ICoverage coverage, final String prefix )
  {
    if( coverage instanceof RectifiedGridCoverage )
    {
      final RectifiedGridCoverage gridCoverage = (RectifiedGridCoverage) coverage;
      final Object rangeSet = gridCoverage.getRangeSet();
      if( rangeSet instanceof FileType )
      {
        final Path filePath = new Path( ((FileType) rangeSet).getFileName() );
        // this works for both flood and risk
        final String fileName = new Path( prefix ).append( filePath.removeFirstSegments( 2 ) ).toString();
        ((FileType) rangeSet).setFileName( fileName );
      }
    }
  }
}
