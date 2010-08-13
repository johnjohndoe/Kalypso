package org.kalypso.calculation.plc.postprocessing;

import java.io.File;
import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import ogc31.www.opengis.net.gml.FileType;

import org.apache.commons.io.FileUtils;
import org.kalypso.calculation.UrlCatalog;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
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
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridCoverage;

public class PLCPostprocessing_Job extends AbstractInternalStatusJob implements ISimulation
{
  private final static QName INUNDATION_STATUSQUO_COVERAGECOLLECTION = new QName( UrlCatalog.NS_CCHAINRESULTS, "inundationStatusQuoCoverageCollection" );

  private final static QName INUNDATION_CALCULATED_COVERAGECOLLECTION = new QName( UrlCatalog.NS_CCHAINRESULTS, "inundationCalculatedCoverageCollection" );

  private final static QName INUNDATION_DIFFERENCE_COVERAGECOLLECTION = new QName( UrlCatalog.NS_CCHAINRESULTS, "inundationDifferenceCoverageCollection" );

  private final static QName RISK_STATUSQUO_COVERAGECOLLECTION = new QName( UrlCatalog.NS_CCHAINRESULTS, "riskStatusQuoCoverageCollection" );

  private final static QName RISK_CALCULATED_COVERAGECOLLECTION = new QName( UrlCatalog.NS_CCHAINRESULTS, "riskCalculatedCoverageCollection" );

  private final static QName RISK_DIFFERENCE_COVERAGECOLLECTION = new QName( UrlCatalog.NS_CCHAINRESULTS, "riskDifferenceCoverageCollection" );

  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( "resources/modelSpecification.xml" );
  }

  public URL getTemplate( )
  {
    return getClass().getResource( "resources/template.gml" );
  }

  public URL getSLD( )
  {
    return getClass().getResource( "resources/RiskZonesCoverage.sld" );
  }

  @Override
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    try
    {
      final GMLWorkspace riskStatusQuoRasterDataModelWS = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( "riskStatusQuoRasterDataModel" ), null );
      final GMLWorkspace riskCalculatedRasterDataModelWS = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( "riskCalculatedRasterDataModel" ), null );
      final GMLWorkspace riskDifferenceRasterDataModelWS = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( "riskDifferenceRasterDataModel" ), null );

      final File naResultsFolder = FileUtils.toFile( (URL) inputProvider.getInputForID( "naResultsFolder" ) );
      final File statisticsFile = FileUtils.toFile( (URL) inputProvider.getInputForID( "riskDifferenceStatistics" ) );
      final File riskStatusQuoRasterFolderInput = FileUtils.toFile( (URL) inputProvider.getInputForID( "riskStatusQuoRasterFolderInput" ) );
      final File riskStatusQuoRasterFolderOutput = FileUtils.toFile( (URL) inputProvider.getInputForID( "riskStatusQuoRasterFolderOutput" ) );
      final File riskCalculatedRasterFolderInput = FileUtils.toFile( (URL) inputProvider.getInputForID( "riskCalculatedRasterFolderInput" ) );
      final File riskCalculatedRasterFolderOutput = FileUtils.toFile( (URL) inputProvider.getInputForID( "riskCalculatedRasterFolderOutput" ) );
      final File riskDifferenceRasterFolderOutput = FileUtils.toFile( (URL) inputProvider.getInputForID( "riskDifferenceRasterFolderOutput" ) );

// final File outputModel = FileUtils.toFile(getTemplate());

      final GMLWorkspace resultsWorkspace = GmlSerializer.createGMLWorkspace( getTemplate(), null );
      final ICoverageCollection inundationStatusQuoCoverageCollection = (ICoverageCollection) ((Feature) resultsWorkspace.getRootFeature().getProperty( INUNDATION_STATUSQUO_COVERAGECOLLECTION )).getAdapter( ICoverageCollection.class );
      final ICoverageCollection inundationCalculatedCoverageCollection = (ICoverageCollection) ((Feature) resultsWorkspace.getRootFeature().getProperty( INUNDATION_CALCULATED_COVERAGECOLLECTION )).getAdapter( ICoverageCollection.class );
      final ICoverageCollection inundationDifferenceCoverageCollection = (ICoverageCollection) ((Feature) resultsWorkspace.getRootFeature().getProperty( INUNDATION_DIFFERENCE_COVERAGECOLLECTION )).getAdapter( ICoverageCollection.class );
      final ICoverageCollection riskStatusQuoCoverageCollection = (ICoverageCollection) ((Feature) resultsWorkspace.getRootFeature().getProperty( RISK_STATUSQUO_COVERAGECOLLECTION )).getAdapter( ICoverageCollection.class );
      final ICoverageCollection riskCalculatedCoverageCollection = (ICoverageCollection) ((Feature) resultsWorkspace.getRootFeature().getProperty( RISK_CALCULATED_COVERAGECOLLECTION )).getAdapter( ICoverageCollection.class );
      final ICoverageCollection riskDifferenceCoverageCollection = (ICoverageCollection) ((Feature) resultsWorkspace.getRootFeature().getProperty( RISK_DIFFERENCE_COVERAGECOLLECTION )).getAdapter( ICoverageCollection.class );

      final IRasterDataModel riskStatusQuoRasterDataModel = (IRasterDataModel) riskStatusQuoRasterDataModelWS.getRootFeature().getAdapter( IRasterDataModel.class );
      final IRasterDataModel riskCalculatedRasterDataModel = (IRasterDataModel) riskCalculatedRasterDataModelWS.getRootFeature().getAdapter( IRasterDataModel.class );
      final IRasterDataModel riskDifferenceRasterDataModel = (IRasterDataModel) riskDifferenceRasterDataModelWS.getRootFeature().getAdapter( IRasterDataModel.class );

      // adding representative (HQ100) inundation coverages to the model
      for( final IAnnualCoverageCollection coverageCollection : riskStatusQuoRasterDataModel.getWaterlevelCoverageCollection() )
      {
        if( coverageCollection.getReturnPeriod() == 100 )
        {
          IFeatureBindingCollection<ICoverage> coverages = coverageCollection.getCoverages();
          for( final ICoverage coverage : coverages )
          {
            changeCoverageFilePathPrefix( coverage, "statusQuo/" + riskStatusQuoRasterFolderInput.getName() );
            inundationStatusQuoCoverageCollection.getCoverages().add( coverage );
          }
        }
      }
      for( final IAnnualCoverageCollection coverageCollection : riskCalculatedRasterDataModel.getWaterlevelCoverageCollection() )
      {
        if( coverageCollection.getReturnPeriod() == 100 )
        {
          IFeatureBindingCollection<ICoverage> coverages = coverageCollection.getCoverages();
          for( final ICoverage coverage : coverages )
          {
            changeCoverageFilePathPrefix( coverage, "calculated/" + riskStatusQuoRasterFolderInput.getName() );
            inundationCalculatedCoverageCollection.getCoverages().add( coverage );
          }
        }
      }
      for( final IAnnualCoverageCollection coverageCollection : riskDifferenceRasterDataModel.getWaterlevelCoverageCollection() )
      {
        if( coverageCollection.getReturnPeriod() == 100 )
        {
          IFeatureBindingCollection<ICoverage> coverages = coverageCollection.getCoverages();
          for( final ICoverage coverage : coverages )
          {
            changeCoverageFilePathPrefix( coverage, "difference/" + riskStatusQuoRasterFolderOutput.getName() );
            inundationDifferenceCoverageCollection.getCoverages().add( coverage );
          }
        }
      }

      // adding risk zones coverages to the model
      for( final ICoverage coverage : riskStatusQuoRasterDataModel.getRiskZonesCoverage().getCoverages() )
      {
        changeCoverageFilePathPrefix( coverage, "statusQuo/" + riskStatusQuoRasterFolderOutput.getName() );
        riskStatusQuoCoverageCollection.getCoverages().add( coverage );
      }
      for( final ICoverage coverage : riskCalculatedRasterDataModel.getRiskZonesCoverage().getCoverages() )
      {
        changeCoverageFilePathPrefix( coverage, "calculated/" + riskCalculatedRasterFolderOutput.getName() );
        riskCalculatedCoverageCollection.getCoverages().add( coverage );
      }
      for( final ICoverage coverage : riskDifferenceRasterDataModel.getRiskZonesCoverage().getCoverages() )
      {
        changeCoverageFilePathPrefix( coverage, "difference/" + riskDifferenceRasterFolderOutput.getName() );
        riskDifferenceCoverageCollection.getCoverages().add( coverage );
      }

      final File naFolder = new File( tmpdir, "rrm" );
      final File riskFolder = new File( tmpdir, "risk" );
      final File riskFolderStatusQuo = new File( riskFolder, "statusQuo" );
      final File riskFolderCalculated = new File( riskFolder, "calculated" );
      final File riskFolderDifference = new File( riskFolder, "difference" );
      final File file = new File( riskFolder, "result.gml" );
      naFolder.mkdirs();
      riskFolder.mkdirs();
      riskFolderStatusQuo.mkdirs();
      riskFolderCalculated.mkdirs();
      riskFolderDifference.mkdirs();
      file.createNewFile();
      GmlSerializer.serializeWorkspace( file, resultsWorkspace, "UTF-8" );

      final File controlModelFile = new File( riskFolder, "RasterizationControlModel.gml" );
      final File sldFile = new File( riskFolder, "RiskZonesCoverage.sld" );
      try
      {
        FileUtils.copyURLToFile( (URL) inputProvider.getInputForID( "riskRasterizationControlModel" ), controlModelFile );
        FileUtils.copyURLToFile( getSLD(), sldFile );
        // FileUtils.copyFileToDirectory( sldFile, riskFolder );
      }
      catch( final Exception e )
      {
        Logger.getAnonymousLogger().log( Level.WARNING, "Could not copy SLD file. Reason: " + e.getLocalizedMessage() );
      }

      FileUtils.copyFileToDirectory( statisticsFile, riskFolder );
      FileUtils.copyDirectoryToDirectory( riskStatusQuoRasterFolderInput, riskFolderStatusQuo );
      FileUtils.copyDirectoryToDirectory( riskStatusQuoRasterFolderOutput, riskFolderStatusQuo );
      FileUtils.copyDirectoryToDirectory( riskCalculatedRasterFolderInput, riskFolderCalculated );
      FileUtils.copyDirectoryToDirectory( riskCalculatedRasterFolderOutput, riskFolderCalculated );
      FileUtils.copyDirectoryToDirectory( riskDifferenceRasterFolderOutput, riskFolderDifference );
      FileUtils.copyDirectoryToDirectory( naResultsFolder, naFolder );

    }
    catch( final Exception e )
    {
      e.printStackTrace();
      Logger.getAnonymousLogger().log( Level.SEVERE, e.getLocalizedMessage() );
      setStatus( STATUS.ERROR, e.getLocalizedMessage() );
    }
    resultEater.addResult( "outputFolder", tmpdir );
    setStatus( STATUS.OK, "Success" );
  }

  private final void changeCoverageFilePathPrefix( final ICoverage coverage, final String prefix )
  {
    if( coverage instanceof RectifiedGridCoverage )
    {
      final RectifiedGridCoverage gridCoverage = (RectifiedGridCoverage) coverage;
      final Object rangeSet = gridCoverage.getRangeSet();
      if( rangeSet instanceof FileType )
      {
        final String fileName = ((FileType) rangeSet).getFileName();
        ((FileType) rangeSet).setFileName( prefix + "/" + fileName.substring( fileName.lastIndexOf( "/" ) + 1 ) );
      }
    }
  }
}
