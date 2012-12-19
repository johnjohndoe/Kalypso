package org.kalypso.calculation.connector.k1d2d_fm;

import java.io.File;
import java.math.BigDecimal;
import java.net.URL;
import java.util.Date;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.kalypso.calculation.connector.utils.Connectors;
import org.kalypso.gml.ui.coverage.CoverageManagementHelper;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.model.flood.binding.IFloodModel;
import org.kalypso.model.flood.binding.IFloodPolygon;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypso.model.flood.binding.ITinReference;
import org.kalypso.model.flood.binding.ITinReference.SOURCETYPE;
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
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverage;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;
import org.kalypsodeegree_impl.gml.binding.commons.NamedFeatureHelper;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;

public class Connector_1D2D_FM_Job extends AbstractInternalStatusJob implements ISimulation
{
  public static final String INPUT_WATER_SURFACE_TIN = "waterSurfaceTin"; //$NON-NLS-1$

  public static final String INPUT_RETURN_PERIOD = "returnPeriod"; //$NON-NLS-1$

  public static final String INPUT_FLOOD_MODEL = "floodModel"; //$NON-NLS-1$

  public static final String INPUT_DELETE_EXISTING_EVENTS = "OPT_DeleteExistingRunoffEvents"; //$NON-NLS-1$

  public static final String OUTPUT_FLOOD_MODEL = "floodModel"; //$NON-NLS-1$

  public static final String ID = "KalypsoModelConnector_1D2D_FM"; //$NON-NLS-1$

  public Connector_1D2D_FM_Job( )
  {
  }

  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( "resources/modelSpecification.xml" ); //$NON-NLS-1$
  }

  @Override
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    try
    {
      final GMLWorkspace tinFile = Connectors.getWorkspace( inputProvider, INPUT_WATER_SURFACE_TIN );
      final GMLWorkspace fmModel = Connectors.getWorkspace( inputProvider, INPUT_FLOOD_MODEL );

      final boolean deleteExistingRunoffEvents = deleteRunOffEvents( inputProvider );
      final int returnPeriod = Integer.parseInt( (String)inputProvider.getInputForID( INPUT_RETURN_PERIOD ) );

      final File fmOutputFile = File.createTempFile( "outTempFM", ".gml", tmpdir ); //$NON-NLS-1$ //$NON-NLS-2$

      final Feature tinRootFeature = tinFile.getRootFeature();
      final IFloodModel floodModel = (IFloodModel)fmModel.getRootFeature().getAdapter( IFloodModel.class );
      final IFeatureBindingCollection<IRunoffEvent> floodModelEvents = floodModel.getEvents();
      if( deleteExistingRunoffEvents )
      {
        for( final IRunoffEvent event : floodModelEvents )
        {
          final ICoverageCollection coverages = event.getResultCoverages();
          final IFeatureBindingCollection<ICoverage> coveragesList = coverages.getCoverages();
          for( final ICoverage coverage : coveragesList )
          {
            CoverageManagementHelper.deleteRangeSetFile( coverage );
          }
        }
        floodModelEvents.clear();
      }
      final String dataPath = String.format( "HQ%d", returnPeriod ); //$NON-NLS-1$
      final IRunoffEvent newRunoffEvent = floodModelEvents.addNew( IRunoffEvent.QNAME );
      newRunoffEvent.setName( newRunoffEvent.getId() );
      newRunoffEvent.setDataPath( new Path( dataPath ) );
      newRunoffEvent.setReturnPeriod( returnPeriod );
      newRunoffEvent.setMarkedForProcessing( true );

      for( final IFloodPolygon polygon : floodModel.getPolygons() )
      {
        if( deleteExistingRunoffEvents )
        {
          polygon.getEvents().clear();
        }
        polygon.getEvents().addRef( newRunoffEvent );
      }
      final ITinReference tinReference = newRunoffEvent.getTins().addNew( ITinReference.QNAME );
      final GM_TriangulatedSurface triangulatedSurface = (GM_TriangulatedSurface)tinRootFeature.getProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "triangulatedSurfaceMember" ) ); //$NON-NLS-1$
      final GMLXPath sourceFeaturePath = new GMLXPath( "TinResult/triangulatedSurfaceMember", tinFile.getNamespaceContext() ); //$NON-NLS-1$

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

      tinReference.setName( NamedFeatureHelper.getName( tinRootFeature ) );
      tinReference.setDescription( NamedFeatureHelper.getDescription( tinRootFeature ) );
      tinReference.setTin( triangulatedSurface );
      tinReference.setMax( BigDecimal.valueOf( max ) );
      tinReference.setMin( BigDecimal.valueOf( min ) );
      tinReference.setSourceFeaturePath( sourceFeaturePath );
      tinReference.setSourceLocation( Connectors.getURL( inputProvider, INPUT_WATER_SURFACE_TIN ) );
      tinReference.setUpdateDate( new Date() );
      tinReference.setSourceType( SOURCETYPE.gml );
      GmlSerializer.serializeWorkspace( fmOutputFile, fmModel, "UTF-8" ); //$NON-NLS-1$
      setStatus( IStatus.OK, "Success" );
      resultEater.addResult( OUTPUT_FLOOD_MODEL, fmOutputFile );
    }
    catch( final Exception e )
    {
      throw new SimulationException( "Problem bei der Übertragung der Wasserspiegellage von Kalypso1D2D zu KalypsoFlood", e );
    }
  }

  private boolean deleteRunOffEvents( final ISimulationDataProvider inputProvider ) throws SimulationException
  {
    if( inputProvider.hasID( INPUT_DELETE_EXISTING_EVENTS ) )
    {
      final String pathString = inputProvider.getInputForID( INPUT_DELETE_EXISTING_EVENTS ).toString();
      return Boolean.parseBoolean( pathString.substring( pathString.lastIndexOf( "/" ) + 1 ) ); //$NON-NLS-1$
    }

    return false;
  }
}
