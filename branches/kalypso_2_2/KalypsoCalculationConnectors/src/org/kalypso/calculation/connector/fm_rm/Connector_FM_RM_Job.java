package org.kalypso.calculation.connector.fm_rm;

import java.io.File;
import java.net.URL;

import ogc31.www.opengis.net.gml.FileType;

import org.apache.commons.io.FileUtils;
import org.kalypso.calculation.connector.AbstractInternalStatusJob;
import org.kalypso.calculation.connector.IKalypsoModelConnectorType.MODELSPEC_CONNECTOR_FM_RM;
import org.kalypso.model.flood.binding.IFloodModel;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridCoverage;

public class Connector_FM_RM_Job extends AbstractInternalStatusJob implements ISimulation
{
  public Connector_FM_RM_Job( )
  {
  }

  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( "resources/modelSpecification.xml" );
  }

  @Override
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    final URL fmModelURL = (URL) inputProvider.getInputForID( MODELSPEC_CONNECTOR_FM_RM.FM_Model.name() );
    final URL rmModelURL = (URL) inputProvider.getInputForID( MODELSPEC_CONNECTOR_FM_RM.RM_Model.name() );
    final String rmInputRasterFolderRelativePath = "raster/input/";
    final File rmInputRasterFolder = new File( tmpdir, "rmInputRasterFolder" );

    try
    {
      final GMLWorkspace fmModel = GmlSerializer.createGMLWorkspace( fmModelURL, null );
      final GMLWorkspace rmModel = GmlSerializer.createGMLWorkspace( rmModelURL, null );

      final File rmOutputFile = File.createTempFile( "outTempRM", ".gml", tmpdir );
      // folder absolute path on server FS
      final String fmScenarioFolderAbsolutePath = new File( fmModelURL.getFile() ).getParent().concat( File.separator );

      final IFloodModel floodModel = (IFloodModel) fmModel.getRootFeature().getAdapter( IFloodModel.class );
      final IRasterDataModel riskRasterDataModel = (IRasterDataModel) rmModel.getRootFeature().getAdapter( IRasterDataModel.class );

      final IFeatureWrapperCollection<IRunoffEvent> floodModelEvents = floodModel.getEvents();
      final IFeatureWrapperCollection<IAnnualCoverageCollection> riskWaterlevelCoverageCollection = riskRasterDataModel.getWaterlevelCoverageCollection();
      riskWaterlevelCoverageCollection.clear();
      for( final IRunoffEvent runoffEvent : floodModelEvents )
      {
        final IAnnualCoverageCollection annualCoverageCollection = riskWaterlevelCoverageCollection.addNew( IAnnualCoverageCollection.QNAME );
        annualCoverageCollection.setName( "[" + runoffEvent.getName() + "]" );
        annualCoverageCollection.setReturnPeriod(runoffEvent.getReturnPeriod());
        final ICoverageCollection coverages = runoffEvent.getResultCoverages();
        for( final ICoverage coverage : coverages )
        {
          if( fmScenarioFolderAbsolutePath != null && coverage instanceof RectifiedGridCoverage )
          {
            final RectifiedGridCoverage rCoverage = (RectifiedGridCoverage) coverage;
            final Object rangeSet = rCoverage.getRangeSet();
            // TODO: support other rangeSet types; possibly put this
            // code into a helper class
            if( rangeSet instanceof FileType )
            {
              final FileType fileType = (FileType) rangeSet;
              final File eventFile = new File( fmScenarioFolderAbsolutePath.concat( fileType.getFileName() ) );
              if( eventFile.exists() && eventFile.isFile() )
              {
                FileUtils.copyFileToDirectory( eventFile, rmInputRasterFolder );
                fileType.setFileName( rmInputRasterFolderRelativePath.concat( eventFile.getName() ) );
              }
            }
          }
          annualCoverageCollection.add( coverage );
        }
      }
      GmlSerializer.serializeWorkspace( rmOutputFile, rmModel, "UTF-8" );
      setStatus( STATUS.OK, "Success" );
// if (isOkStatus())
      resultEater.addResult( MODELSPEC_CONNECTOR_FM_RM.RM_Model.name(), rmOutputFile );
      resultEater.addResult( MODELSPEC_CONNECTOR_FM_RM.RM_InputRasterFolder.name(), rmInputRasterFolder );
    }
    catch( final Exception e )
    {
      setStatus( STATUS.ERROR, e.getLocalizedMessage() );
      e.printStackTrace();
    }
  }
}
