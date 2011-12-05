package org.kalypso.calculation.connector.wspm_fm;

import java.io.File;
import java.math.BigDecimal;
import java.net.URL;

import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;

import org.eclipse.core.runtime.Path;
import org.kalypso.calculation.connector.IKalypsoModelConnectorType.MODELSPEC_CONNECTOR_WSPM_FM;
import org.kalypso.gml.ui.map.CoverageManagementHelper;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.model.flood.binding.IFloodModel;
import org.kalypso.model.flood.binding.IFloodPolygon;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypso.model.flood.binding.ITinReference;
import org.kalypso.model.flood.binding.ITinReference.SOURCETYPE;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
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
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;
import org.kalypsodeegree_impl.gml.binding.commons.NamedFeatureHelper;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;

public class Connector_WSPM_FM_Job extends AbstractInternalStatusJob implements ISimulation
{
  public Connector_WSPM_FM_Job( )
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
    final URL wspmTinFileURL = (URL) inputProvider.getInputForID( MODELSPEC_CONNECTOR_WSPM_FM.WSPM_TinFile.name() );
    final URL wspmModelURL = (URL) inputProvider.getInputForID( MODELSPEC_CONNECTOR_WSPM_FM.WSPM_Model.name() );
    final URL fmModelURL = (URL) inputProvider.getInputForID( MODELSPEC_CONNECTOR_WSPM_FM.FM_Model.name() );

    final String runOffEventID = inputProvider.getInputForID( MODELSPEC_CONNECTOR_WSPM_FM.WSPM_RunoffEventID.name() ).toString();
    boolean deleteExistingRunoffEvents = false;
    if( inputProvider.hasID( MODELSPEC_CONNECTOR_WSPM_FM.OPT_DeleteExistingRunoffEvents.name() ) )
    {
      final String pathString = inputProvider.getInputForID( MODELSPEC_CONNECTOR_WSPM_FM.OPT_DeleteExistingRunoffEvents.name() ).toString();
      deleteExistingRunoffEvents = Boolean.parseBoolean( pathString.substring( pathString.lastIndexOf( "/" ) + 1 ) );
    }

    try
    {
      final GMLWorkspace wspmTinFile = GmlSerializer.createGMLWorkspace( wspmTinFileURL, null );
      final GMLWorkspace wspmModel = GmlSerializer.createGMLWorkspace( wspmModelURL, null );
      final GMLWorkspace fmModel = GmlSerializer.createGMLWorkspace( fmModelURL, null );
      final File fmOutputFile = File.createTempFile( "outTempFM", ".gml", tmpdir );

      // final File fmEventsFolder = new File(((URL)
      // inputProvider.getInputForID(MODELSPEC_CONNECTOR_WSPM_FM.FM_EventsFolder.name())).toExternalForm());

      Integer returnPeriod = null;
      final IFeatureType wspmRunoffEventFeatureType = wspmModel.getGMLSchema().getFeatureType( new QName( IWspmConstants.NS_WSPMRUNOFF, "RunOffEvent" ) );
      final Feature[] wspmRunoffEvents = wspmModel.getFeatures( wspmRunoffEventFeatureType );
      for( final Feature feature : wspmRunoffEvents )
      {
        if( runOffEventID.equals( feature.getId() ) )
        {
          returnPeriod = (Integer) feature.getProperty( new QName( IWspmConstants.NS_WSPMRUNOFF, "returnPeriod" ) );
          break;
        }
      }
      if( returnPeriod == null )
      {
        // TODO: throw an Exception here?
        returnPeriod = 1;
      }

      final Feature wspmTinRootFeature = wspmTinFile.getRootFeature();
      final IFloodModel floodModel = (IFloodModel) fmModel.getRootFeature().getAdapter( IFloodModel.class );
      final IFeatureWrapperCollection<IRunoffEvent> floodModelEvents = floodModel.getEvents();
      if( deleteExistingRunoffEvents )
      {
        for( final IRunoffEvent event : floodModelEvents )
        {
          final ICoverageCollection coverages = event.getResultCoverages();
          IFeatureBindingCollection<ICoverage> coveragesList = coverages.getCoverages();
          for( final ICoverage coverage : coveragesList )
            CoverageManagementHelper.deleteGridFile( coverage );
        }
        floodModelEvents.clear();
      }
      final IRunoffEvent newRunoffEvent = floodModelEvents.addNew( IRunoffEvent.QNAME );
      newRunoffEvent.setName( newRunoffEvent.getGmlID() );
      newRunoffEvent.setDataPath( new Path( newRunoffEvent.getGmlID() ) );
      newRunoffEvent.setReturnPeriod( returnPeriod );
      newRunoffEvent.setMarkedForProcessing( true );

      for( final IFloodPolygon polygon : floodModel.getPolygons() )
      {
        if( deleteExistingRunoffEvents )
          polygon.getEvents().clear();
        polygon.getEvents().addRef( newRunoffEvent );
      }
      final ITinReference tinReference = newRunoffEvent.getTins().addNew( ITinReference.QNAME );
      final GM_TriangulatedSurface triangulatedSurface = (GM_TriangulatedSurface) wspmTinRootFeature.getProperty( new QName( IWspmConstants.NS_WSPMCOMMONS, "triangulatedSurfaceMember" ) );
      final XMLGregorianCalendar date = (XMLGregorianCalendar) wspmTinRootFeature.getProperty( new QName( IWspmConstants.NS_WSPMCOMMONS, "date" ) );
      final GMLXPath sourceFeaturePath = new GMLXPath( "TriangulatedSurfaceFeature/triangulatedSurfaceMember", wspmTinFile.getNamespaceContext() );

      double min = Double.MAX_VALUE;
      double max = -Double.MAX_VALUE;
      for( final GM_Triangle triangle : triangulatedSurface )
      {
        for( final GM_Position position : triangle.getExteriorRing() )
        {
          final double[] array = position.getAsArray();
          if( array.length < 3 )
          {
            continue;
          }
          min = Math.min( min, array[2] );
          max = Math.max( max, array[2] );
        }
      }

      tinReference.setName( NamedFeatureHelper.getName( wspmTinRootFeature ) );
      tinReference.setDescription( NamedFeatureHelper.getDescription( wspmTinRootFeature ) );
      tinReference.setTin( triangulatedSurface );
      tinReference.setMax( BigDecimal.valueOf( max ) );
      tinReference.setMin( BigDecimal.valueOf( min ) );
      tinReference.setSourceFeaturePath( sourceFeaturePath );
      tinReference.setSourceLocation( wspmTinFileURL );
      tinReference.setUpdateDate( date.toGregorianCalendar().getTime() );
      tinReference.setSourceType( SOURCETYPE.gml );
      GmlSerializer.serializeWorkspace( fmOutputFile, fmModel, "UTF-8" );
      setStatus( STATUS.OK, "Success" );
      // if (isOkStatus())
      resultEater.addResult( MODELSPEC_CONNECTOR_WSPM_FM.FM_Model.name(), fmOutputFile );
      // resultEater.addResult(MODELSPEC_CONNECTOR_WSPM_FM.FM_EventsFolder.name(),
      // fmEventsFolder);
    }
    catch( final Exception e )
    {
      setStatus( STATUS.ERROR, e.getLocalizedMessage() );
      e.printStackTrace();
    }
  }
}
