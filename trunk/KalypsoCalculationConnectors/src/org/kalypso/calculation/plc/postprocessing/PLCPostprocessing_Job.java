package org.kalypso.calculation.plc.postprocessing;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.kalypso.calculation.plc.postprocessing.binding.IScenarioResults;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
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
import org.kalypsodeegree.model.coverage.RangeSetFile;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridCoverage;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

public class PLCPostprocessing_Job extends AbstractInternalStatusJob implements ISimulation
{
  public static final String ID = "KalypsoPLC_Postprocessing"; //$NON-NLS-1$

  // flood inputs
  public static final String INPUT_FLOOD_DIFFERENCE_MODEL = "floodDifferenceModel"; //$NON-NLS-1$

  public static final String INPUT_FLOOD_DIFFERENCE_RESULT_FOLDER = "floodDifferenceResultFolder"; //$NON-NLS-1$

  public static final String INPUT_FLOOD_CALCULATED_MODEL = "floodCalculatedModel"; //$NON-NLS-1$

  public static final String INPUT_FLOOD_CALCULATED_RESULT_FOLDER = "floodCalculatedResultFolder"; //$NON-NLS-1$

  public static final String INPUT_FLOOD_STATUS_QUO_MODEL = "floodStatusQuoModel"; //$NON-NLS-1$

  public static final String INPUT_FLOOD_STATUS_QUO_RESULT_FOLDER = "floodStatusQuoResultFolder"; //$NON-NLS-1$

  // na inputs
  public static final String INPUT_NA_RESULTS_FOLDER = "naResultsFolder"; //$NON-NLS-1$

  // length section inputs
  public static final String INPUT_LENGTH_SECTION_STATUS_QUO_FOLDER = "lengthSectionStatusQuoFolder"; //$NON-NLS-1$

  public static final String INPUT_LENGTH_SECTION_CALCULATED_FOLDER = "lengthSectionCalculatedFolder"; //$NON-NLS-1$

  public static final String INPUT_LENGTH_SECTION_DIFFERENCE_FOLDER = "lengthSectionDifferenceFolder"; //$NON-NLS-1$

  // risk inputs
  public static final String INPUT_RISK_RASTERIZATION_CONTROL_MODEL = "riskRasterizationControlModel"; //$NON-NLS-1$

  public static final String INPUT_RISK_DIFFERENCE_RASTER_DATA_MODEL = "riskDifferenceRasterDataModel"; //$NON-NLS-1$

  public static final String INPUT_RISK_CALCULATED_RASTER_DATA_MODEL = "riskCalculatedRasterDataModel"; //$NON-NLS-1$

  public static final String INPUT_RISK_DIFFERENCE_RASTER_FOLDER_OUTPUT = "riskDifferenceRasterFolderOutput"; //$NON-NLS-1$

  public static final String INPUT_RISK_CALCULATED_RASTER_FOLDER_OUTPUT = "riskCalculatedRasterFolderOutput"; //$NON-NLS-1$

  public static final String INPUT_RISK_STATUS_QUO_RASTER_FOLDER_OUTPUT = "riskStatusQuoRasterFolderOutput"; //$NON-NLS-1$

  public static final String INPUT_RISK_DIFFERENCE_STATISTICS = "riskDifferenceStatistics"; //$NON-NLS-1$

  public static final String INPUT_RISK_STATUS_QUO_RASTER_DATA_MODEL = "riskStatusQuoRasterDataModel"; //$NON-NLS-1$

  // output folder
  public static final String OUTPUT_FOLDER = "outputFolder"; //$NON-NLS-1$

  private static final FileFilter GML_FILE_FILTER = new FileFilter()
  {
    @Override
    public boolean accept( final File pathname )
    {
      return pathname.getName().endsWith( ".gml" ); //$NON-NLS-1$
    }
  };

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
      new File( outputFolder, "_doNotDelete.txt" ).createNewFile(); //$NON-NLS-1$

      final GMLWorkspace resultsWorkspace = GmlSerializer.createGMLWorkspace( getTemplate(), null );

      addNAResults( inputProvider, outputFolder );
      addLengthSectionResults( resultsWorkspace, inputProvider, outputFolder );
      addInundationResults( resultsWorkspace, inputProvider, outputFolder );
      addRiskResults( resultsWorkspace, inputProvider, outputFolder );

      final File file = new File( outputFolder, "result.gml" ); //$NON-NLS-1$
      GmlSerializer.serializeWorkspace( file, resultsWorkspace, "UTF-8" ); //$NON-NLS-1$
    }
    catch( final Exception e )
    {
      Logger.getAnonymousLogger().log( Level.SEVERE, e.getLocalizedMessage() );
      throw new SimulationException( "Problem bei der Verarbeitung der Ergebnisse der Prozesskette", e );
    }
    resultEater.addResult( OUTPUT_FOLDER, outputFolder ); //$NON-NLS-1$
    setStatus( IStatus.OK, "Success" );
  }

  private void addNAResults( final ISimulationDataProvider inputProvider, final File outputFolder ) throws SimulationException, IOException
  {
    if( !inputProvider.hasID( INPUT_NA_RESULTS_FOLDER ) )
      return;

    final File naResultsFolder = FileUtils.toFile( (URL)inputProvider.getInputForID( INPUT_NA_RESULTS_FOLDER ) );
    final File naFolder = new File( outputFolder, "rrm" ); //$NON-NLS-1$
    naFolder.mkdirs();
    FileUtils.moveDirectoryToDirectory( naResultsFolder, naFolder, false );
  }

  private void addRiskResults( final GMLWorkspace resultsWorkspace, final ISimulationDataProvider inputProvider, final File outputFolder ) throws SimulationException, Exception, IOException, GmlSerializeException
  {
    if( !inputProvider.hasID( INPUT_RISK_STATUS_QUO_RASTER_DATA_MODEL ) )
      return;

    // adding risk zones coverages to the model
    final GMLWorkspace riskStatusQuoRasterDataModelWS = GmlSerializer.createGMLWorkspace( (URL)inputProvider.getInputForID( INPUT_RISK_STATUS_QUO_RASTER_DATA_MODEL ), null );
    final IRasterDataModel riskStatusQuoRasterDataModel = (IRasterDataModel)riskStatusQuoRasterDataModelWS.getRootFeature().getAdapter( IRasterDataModel.class );
    final File riskStatusQuoRasterFolderOutput = FileUtils.toFile( (URL)inputProvider.getInputForID( INPUT_RISK_STATUS_QUO_RASTER_FOLDER_OUTPUT ) );
    final ICoverageCollection riskStatusQuoCoverageCollection = (ICoverageCollection)((Feature)resultsWorkspace.getRootFeature().getProperty( IScenarioResults.QN_PROPERTY_RISK_STATUS_QUO_COVERAGES )).getAdapter( ICoverageCollection.class );
    for( final ICoverage coverage : riskStatusQuoRasterDataModel.getRiskZonesCoverage().getCoverages() )
    {
      changeCoverageFilePathPrefix( coverage, "statusQuo/" + riskStatusQuoRasterFolderOutput.getName() ); //$NON-NLS-1$
      riskStatusQuoCoverageCollection.getCoverages().add( coverage );
    }

    final GMLWorkspace riskCalculatedRasterDataModelWS = GmlSerializer.createGMLWorkspace( (URL)inputProvider.getInputForID( INPUT_RISK_CALCULATED_RASTER_DATA_MODEL ), null );
    final IRasterDataModel riskCalculatedRasterDataModel = (IRasterDataModel)riskCalculatedRasterDataModelWS.getRootFeature().getAdapter( IRasterDataModel.class );
    final ICoverageCollection riskCalculatedCoverageCollection = (ICoverageCollection)((Feature)resultsWorkspace.getRootFeature().getProperty( IScenarioResults.QN_PROPERTY_RISK_CALCULATED_COVERAGES )).getAdapter( ICoverageCollection.class );
    final File riskCalculatedRasterFolderOutput = FileUtils.toFile( (URL)inputProvider.getInputForID( INPUT_RISK_CALCULATED_RASTER_FOLDER_OUTPUT ) );
    for( final ICoverage coverage : riskCalculatedRasterDataModel.getRiskZonesCoverage().getCoverages() )
    {
      changeCoverageFilePathPrefix( coverage, "calculated/" + riskCalculatedRasterFolderOutput.getName() ); //$NON-NLS-1$
      riskCalculatedCoverageCollection.getCoverages().add( coverage );
    }

    final GMLWorkspace riskDifferenceRasterDataModelWS = GmlSerializer.createGMLWorkspace( (URL)inputProvider.getInputForID( INPUT_RISK_DIFFERENCE_RASTER_DATA_MODEL ), null );
    final IRasterDataModel riskDifferenceRasterDataModel = (IRasterDataModel)riskDifferenceRasterDataModelWS.getRootFeature().getAdapter( IRasterDataModel.class );
    final ICoverageCollection riskDifferenceCoverageCollection = (ICoverageCollection)((Feature)resultsWorkspace.getRootFeature().getProperty( IScenarioResults.QN_PROPERTY_RISK_DIFFERENCES_COVERAGES )).getAdapter( ICoverageCollection.class );
    final File riskDifferenceRasterFolderOutput = FileUtils.toFile( (URL)inputProvider.getInputForID( INPUT_RISK_DIFFERENCE_RASTER_FOLDER_OUTPUT ) );
    for( final ICoverage coverage : riskDifferenceRasterDataModel.getRiskZonesCoverage().getCoverages() )
    {
      changeCoverageFilePathPrefix( coverage, "difference/" + riskDifferenceRasterFolderOutput.getName() ); //$NON-NLS-1$
      riskDifferenceCoverageCollection.getCoverages().add( coverage );
    }

    final File riskFolder = new File( outputFolder, "risk" ); //$NON-NLS-1$
    riskFolder.mkdir();
    final File file = new File( riskFolder, "result.gml" ); //$NON-NLS-1$
    GmlSerializer.serializeWorkspace( file, resultsWorkspace, "UTF-8" ); //$NON-NLS-1$

    final File controlModelFile = new File( riskFolder, "RasterizationControlModel.gml" ); //$NON-NLS-1$
    final File sldFile = new File( riskFolder, "RiskZonesCoverage.sld" ); //$NON-NLS-1$
    try
    {
      FileUtils.copyURLToFile( (URL)inputProvider.getInputForID( INPUT_RISK_RASTERIZATION_CONTROL_MODEL ), controlModelFile ); //$NON-NLS-1$
      FileUtils.copyURLToFile( getSLD(), sldFile );
      // FileUtils.copyFileToDirectory( sldFile, riskFolder );
    }
    catch( final Exception e )
    {
      Logger.getAnonymousLogger().log( Level.WARNING, "Could not copy SLD file. Reason: " + e.getLocalizedMessage() );
    }

    final File statisticsFile = FileUtils.toFile( (URL)inputProvider.getInputForID( INPUT_RISK_DIFFERENCE_STATISTICS ) );
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
    final File floodStatusQuoRasterFolderInput = FileUtils.toFile( (URL)inputProvider.getInputForID( INPUT_FLOOD_STATUS_QUO_RESULT_FOLDER ) ); //$NON-NLS-1$
    FileUtils.moveDirectoryToDirectory( floodStatusQuoRasterFolderInput, floodFolderStatusQuo, false );

    final GMLWorkspace floodStatusQuoWS = GmlSerializer.createGMLWorkspace( (URL)inputProvider.getInputForID( INPUT_FLOOD_STATUS_QUO_MODEL ), null ); //$NON-NLS-1$
    final IFloodModel floodStatusQuoModel = (IFloodModel)floodStatusQuoWS.getRootFeature().getAdapter( IFloodModel.class );
    final IFeatureBindingCollection<IRunoffEvent> statusQuoEvents = floodStatusQuoModel.getEvents();

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

    final ICoverageCollection inundationStatusQuoCoverageCollection = (ICoverageCollection)((Feature)resultsWorkspace.getRootFeature().getProperty( IScenarioResults.QN_PROPERTY_INUNDATION_STATUS_QUO_COVERAGES )).getAdapter( ICoverageCollection.class );
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
    final File floodCalculatedRasterFolderInput = FileUtils.toFile( (URL)inputProvider.getInputForID( INPUT_FLOOD_CALCULATED_RESULT_FOLDER ) ); //$NON-NLS-1$
    FileUtils.moveDirectoryToDirectory( floodCalculatedRasterFolderInput, floodFolderCalculated, false );

    final GMLWorkspace floodCalculatedWS = GmlSerializer.createGMLWorkspace( (URL)inputProvider.getInputForID( INPUT_FLOOD_CALCULATED_MODEL ), null ); //$NON-NLS-1$
    final IFloodModel floodCalculatedModel = (IFloodModel)floodCalculatedWS.getRootFeature().getAdapter( IFloodModel.class );
    final ICoverageCollection inundationCalculatedCoverageCollection = (ICoverageCollection)((Feature)resultsWorkspace.getRootFeature().getProperty( IScenarioResults.QN_PROPERTY_INUNDATION_CALCULATED_COVERAGES )).getAdapter( ICoverageCollection.class );
    final String calculatedPath = "flood/calculated/events"; //$NON-NLS-1$
    final IFeatureBindingCollection<IRunoffEvent> calculatedEvents = floodCalculatedModel.getEvents();
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
    final File floodDifferenceRasterFolderInput = FileUtils.toFile( (URL)inputProvider.getInputForID( INPUT_FLOOD_DIFFERENCE_RESULT_FOLDER ) ); //$NON-NLS-1$
    FileUtils.moveDirectoryToDirectory( floodDifferenceRasterFolderInput, floodFolderDifference, false );

    final GMLWorkspace floodDifferenceWS = GmlSerializer.createGMLWorkspace( (URL)inputProvider.getInputForID( INPUT_FLOOD_DIFFERENCE_MODEL ), null ); //$NON-NLS-1$
    final IFloodModel floodDifferenceModel = (IFloodModel)floodDifferenceWS.getRootFeature().getAdapter( IFloodModel.class );
    final ICoverageCollection inundationDifferenceCoverageCollection = (ICoverageCollection)((Feature)resultsWorkspace.getRootFeature().getProperty( IScenarioResults.QN_PROPERTY_INUNDATION_DIFFERENCES_COVERAGES )).getAdapter( ICoverageCollection.class );
    final String differencePath = "flood/difference/events"; //$NON-NLS-1$
    final IFeatureBindingCollection<IRunoffEvent> differenceEvents = floodDifferenceModel.getEvents();
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

  private void addLengthSectionResults( final GMLWorkspace resultsWorkspace, final ISimulationDataProvider inputProvider, final File outputFolder ) throws Exception
  {
    if( !inputProvider.hasID( INPUT_LENGTH_SECTION_STATUS_QUO_FOLDER ) )
      return;

    // save template
    final File lengthSectionFolder = new File( outputFolder, "lengthSection" ); //$NON-NLS-1$
    final InputStream template = PLCPostprocessing_Job.class.getResourceAsStream( "resources/lengthsection.gft" );//$NON-NLS-1$
    FileUtils.copyInputStreamToFile( template, new File( lengthSectionFolder, "lengthsection.gft" ) ); //$NON-NLS-1$

    // STATUS QUO
    final File lengthSectionFolderStatusQuo = new File( lengthSectionFolder, "statusQuo" ); //$NON-NLS-1$
    final File inputLengthSectionStatusQuo = FileUtils.toFile( (URL)inputProvider.getInputForID( INPUT_LENGTH_SECTION_STATUS_QUO_FOLDER ) );

    // CALCULATED
    final File lengthSectionFolderCalculated = new File( lengthSectionFolder, "calculated" ); //$NON-NLS-1$
    final File inputLengthSectionCalculated = FileUtils.toFile( (URL)inputProvider.getInputForID( INPUT_LENGTH_SECTION_CALCULATED_FOLDER ) );

    // DIFFERENCE
    final File lengthSectionFolderDifference = new File( lengthSectionFolder, "difference" ); //$NON-NLS-1$
    final File inputLengthSectionDifference = FileUtils.toFile( (URL)inputProvider.getInputForID( INPUT_LENGTH_SECTION_DIFFERENCE_FOLDER ) );
    FileUtils.copyDirectory( inputLengthSectionDifference, lengthSectionFolderDifference, GML_FILE_FILTER );

    // store in results GML
    final IScenarioResults scenarioResults = (IScenarioResults)resultsWorkspace.getRootFeature();
    final Feature resultCollection = (Feature)scenarioResults.getProperty( IScenarioResults.QN_PROPERTY_LENGTH_SECTION_RESULT_COLLECTION );
    for( final File file : lengthSectionFolderDifference.listFiles() )
    {
      final String fileName = file.getName();
      final File srcFile = new File( inputLengthSectionCalculated, fileName );
      FileUtils.copyFileToDirectory( new File( inputLengthSectionStatusQuo, fileName ), lengthSectionFolderStatusQuo );
      FileUtils.copyFileToDirectory( new File( inputLengthSectionCalculated, fileName ), lengthSectionFolderCalculated );

      final Feature resultMember = FeatureHelper.addFeature( resultCollection, IScenarioResults.QN_PROPERTY_LENGTH_SECTION_RESULT_MEMBER, IScenarioResults.QN_TYPE_RESULT_MEMBER );
      addObservationLink( "lengthSection/statusQuo/" + fileName, resultMember, IScenarioResults.QN_PROPERTY_STATUS_QUO_MEMBER ); //$NON-NLS-1$
      addObservationLink( "lengthSection/calculated/" + fileName, resultMember, IScenarioResults.QN_PROPERTY_CALCULATED_MEMBER ); //$NON-NLS-1$
      addObservationLink( "lengthSection/difference/" + fileName, resultMember, IScenarioResults.QN_PROPERTY_DIFFERENCE_MEMBER ); //$NON-NLS-1$

      // get name from result
      final GMLWorkspace obsWorkspace = GmlSerializer.createGMLWorkspace( srcFile, null );
      resultMember.setName( obsWorkspace.getRootFeature().getName() );
    }
  }

  private void addObservationLink( final String fileRef, final Feature resultMember, final QName property )
  {
    final String href = fileRef + "#LengthSectionResult"; //$NON-NLS-1$
    final IFeatureType lcFT = resultMember.getFeatureType();
    final IRelationType pt = (IRelationType)lcFT.getProperty( property );

    resultMember.setLink( pt, href, lcFT );
  }

  private void changeCoverageFilePathPrefix( final ICoverage coverage, final String prefix )
  {
    if( coverage instanceof RectifiedGridCoverage )
    {
      final RectifiedGridCoverage gridCoverage = (RectifiedGridCoverage)coverage;
      final Object rangeSet = gridCoverage.getRangeSet();
      if( rangeSet instanceof RangeSetFile )
      {
        final RangeSetFile file = (RangeSetFile)rangeSet;
        final Path filePath = new Path( file.getFileName() );
        // this works for both flood and risk
        final String fileName = new Path( prefix ).append( filePath.removeFirstSegments( 2 ) ).toString();
        file.setFileName( fileName );
      }
    }
  }
}
