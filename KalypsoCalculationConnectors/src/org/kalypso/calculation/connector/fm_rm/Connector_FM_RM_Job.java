package org.kalypso.calculation.connector.fm_rm;

import java.io.File;
import java.net.URL;
import java.util.List;

import javax.xml.bind.JAXBElement;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.calculation.connector.IKalypsoModelConnectorType.MODELSPEC_CONNECTOR_FM_RM;
import org.kalypso.calculation.connector.utils.Connectors;
import org.kalypso.model.flood.binding.IFloodModel;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.simulation.ISimulationSpecKalypsoRisk.MODELSPEC_KALYPSORISK;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypso.risk.model.utils.RiskModelHelper.LAYER_TYPE;
import org.kalypso.simulation.core.AbstractInternalStatusJob;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gismapview.Gismapview.Layers;
import org.kalypso.template.gismapview.ObjectFactory;
import org.kalypso.template.types.StyledLayerType;
import org.kalypsodeegree.model.coverage.RangeSetFile;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
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
    return getClass().getResource( "resources/modelSpecification.xml" ); //$NON-NLS-1$
  }

  @Override
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor )
  {
    final GMLWorkspace fmModel = Connectors.getWorkspace( inputProvider, MODELSPEC_CONNECTOR_FM_RM.FM_Model.name() );
    final GMLWorkspace rmModel = Connectors.getWorkspace( inputProvider, MODELSPEC_CONNECTOR_FM_RM.RM_Model.name() );

    final String rmInputRasterFolderRelativePath = "raster/input/"; //$NON-NLS-1$
    final File rmInputRasterFolder = new File( tmpdir, "rmInputRasterFolder" ); //$NON-NLS-1$

    try
    {

      final File rmOutputFile = File.createTempFile( "outTempRM", ".gml", tmpdir ); //$NON-NLS-1$ //$NON-NLS-2$
      // folder absolute path on server FS
      final String fmScenarioFolderAbsolutePath = new File( Connectors.getURL( inputProvider, MODELSPEC_CONNECTOR_FM_RM.FM_Model.name() ).getFile() ).getParent().concat( File.separator );

      final IFloodModel floodModel = (IFloodModel)fmModel.getRootFeature().getAdapter( IFloodModel.class );
      final IRasterDataModel riskRasterDataModel = (IRasterDataModel)rmModel.getRootFeature().getAdapter( IRasterDataModel.class );

      final IFeatureBindingCollection<IRunoffEvent> floodModelEvents = floodModel.getEvents();
      final IFeatureBindingCollection<IAnnualCoverageCollection> riskWaterlevelCoverageCollection = riskRasterDataModel.getWaterlevelCoverageCollection();
      riskWaterlevelCoverageCollection.clear();
      for( final IRunoffEvent runoffEvent : floodModelEvents )
      {
        final IAnnualCoverageCollection annualCoverageCollection = riskWaterlevelCoverageCollection.addNew( IAnnualCoverageCollection.QNAME );
        annualCoverageCollection.setName( "[" + runoffEvent.getName() + "]" ); //$NON-NLS-1$ //$NON-NLS-2$
        annualCoverageCollection.setReturnPeriod( runoffEvent.getReturnPeriod() );
        final ICoverageCollection coverages = runoffEvent.getResultCoverages();
        final IFeatureBindingCollection<ICoverage> coveragesList = coverages.getCoverages();
        for( final ICoverage coverage : coveragesList )
        {
          if( fmScenarioFolderAbsolutePath != null && coverage instanceof RectifiedGridCoverage )
          {
            final RectifiedGridCoverage rCoverage = (RectifiedGridCoverage)coverage;
            final Object rangeSet = rCoverage.getRangeSet();
            // TODO: support other rangeSet types; possibly put this
            // code into a helper class
            if( rangeSet instanceof RangeSetFile )
            {
              final RangeSetFile fileType = (RangeSetFile)rangeSet;
              final File eventFile = new File( fmScenarioFolderAbsolutePath.concat( fileType.getFileName() ) );
              if( eventFile.exists() && eventFile.isFile() )
              {
                // TODO local or server based calculation chain? server based -> move files to destination directory
                FileUtils.copyFileToDirectory( eventFile, rmInputRasterFolder );
                fileType.setFileName( rmInputRasterFolderRelativePath.concat( eventFile.getName() ) );
              }
            }
          }
          annualCoverageCollection.getCoverages().add( coverage );
        }
      }
      GmlSerializer.serializeWorkspace( rmOutputFile, rmModel, "UTF-8" ); //$NON-NLS-1$
      resultEater.addResult( MODELSPEC_CONNECTOR_FM_RM.RM_Model.name(), rmOutputFile );
      resultEater.addResult( MODELSPEC_CONNECTOR_FM_RM.RM_InputRasterFolder.name(), rmInputRasterFolder );

      final boolean updateMap = inputProvider.hasID( MODELSPEC_KALYPSORISK.MAP_WATERLEVEL.toString() );
      if( updateMap )
      {
        final URL mapURL = (URL)inputProvider.getInputForID( MODELSPEC_KALYPSORISK.MAP_WATERLEVEL.toString() );
        final File mapFile = new File( mapURL.getPath() );

        /* Load the map template. */
        final Gismapview gisview = GisTemplateHelper.loadGisMapView( mapFile );
        final Layers layers = gisview.getLayers();
        final List<JAXBElement< ? extends StyledLayerType>> layersList = layers.getLayer();
        layersList.clear();

        final ObjectFactory layerObjectFactory = new ObjectFactory();
        for( final IAnnualCoverageCollection annualCoverageCollection : riskWaterlevelCoverageCollection )
        {
          final StyledLayerType layer = RiskModelHelper.createMapLayer( LAYER_TYPE.WATERLEVEL, annualCoverageCollection );
          final JAXBElement<StyledLayerType> jaxbLayer = layerObjectFactory.createLayer( layer );
          layersList.add( jaxbLayer );
        }
        final File outMap = File.createTempFile( "tempMap", ".gml", tmpdir ); //$NON-NLS-1$ //$NON-NLS-2$
        GisTemplateHelper.saveGisMapView( gisview, outMap, "UTF-8" ); //$NON-NLS-1$
        resultEater.addResult( MODELSPEC_KALYPSORISK.MAP_WATERLEVEL.toString(), outMap );
      }

      setStatus( IStatus.OK, "Success" );
    }
    catch( final Exception e )
    {
      setStatus( IStatus.ERROR, e.getLocalizedMessage() );
      e.printStackTrace();
    }
  }
}
