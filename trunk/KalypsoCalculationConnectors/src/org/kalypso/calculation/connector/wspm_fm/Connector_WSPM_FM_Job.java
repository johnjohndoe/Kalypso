package org.kalypso.calculation.connector.wspm_fm;

import java.io.File;
import java.math.BigDecimal;
import java.net.URL;

import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.kalypso.calculation.connector.IKalypsoModelConnectorType.MODELSPEC_CONNECTOR_WSPM_FM;
import org.kalypso.calculation.connector.utils.Connectors;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.gml.ui.coverage.CoverageManagementHelper;
import org.kalypso.model.flood.binding.IFloodModel;
import org.kalypso.model.flood.binding.IFloodPolygon;
import org.kalypso.model.flood.binding.IRunoffEvent;
import org.kalypso.model.flood.binding.ITinReference;
import org.kalypso.model.flood.binding.ITinReference.SOURCETYPE;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.IRunOffEvent;
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

public class Connector_WSPM_FM_Job extends AbstractInternalStatusJob implements ISimulation
{
  public Connector_WSPM_FM_Job( )
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

    final GMLWorkspace wspmTinFile = Connectors.getWorkspace( inputProvider, MODELSPEC_CONNECTOR_WSPM_FM.WSPM_TinFile.name() );
    final GMLWorkspace wspmModel = Connectors.getWorkspace( inputProvider, MODELSPEC_CONNECTOR_WSPM_FM.WSPM_Model.name() );
    final GMLWorkspace fmModel = Connectors.getWorkspace( inputProvider, MODELSPEC_CONNECTOR_WSPM_FM.FM_Model.name() );

    final IRunOffEvent runOffEvent = Connectors.findRunOffEvent( wspmModel, inputProvider );
    if( Objects.isNull( runOffEvent ) )
      throw new SimulationException( "Undefined WSPM runoff event ID specified." );

    final boolean deleteExistingRunoffEvents = deleteRunOffEvents( inputProvider );

    Integer returnPeriod = runOffEvent.getAnnuality();
    if( returnPeriod == null )
    {
      // TODO: throw an Exception here?
      returnPeriod = 1;
    }

    try
    {

      final File fmOutputFile = File.createTempFile( "outTempFM", ".gml", tmpdir ); //$NON-NLS-1$ //$NON-NLS-2$

      final Feature wspmTinRootFeature = wspmTinFile.getRootFeature();
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
      final IRunoffEvent newRunoffEvent = floodModelEvents.addNew( IRunoffEvent.QNAME );
      newRunoffEvent.setName( newRunoffEvent.getId() );
      newRunoffEvent.setDataPath( new Path( newRunoffEvent.getId() ) );
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
      final GM_TriangulatedSurface triangulatedSurface = (GM_TriangulatedSurface)wspmTinRootFeature.getProperty( new QName( IWspmConstants.NS_WSPMCOMMONS, "triangulatedSurfaceMember" ) ); //$NON-NLS-1$
      final XMLGregorianCalendar date = (XMLGregorianCalendar)wspmTinRootFeature.getProperty( new QName( IWspmConstants.NS_WSPMCOMMONS, "date" ) ); //$NON-NLS-1$
      final GMLXPath sourceFeaturePath = new GMLXPath( "TriangulatedSurfaceFeature/triangulatedSurfaceMember", wspmTinFile.getNamespaceContext() ); //$NON-NLS-1$

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
      tinReference.setSourceLocation( Connectors.getURL( inputProvider, MODELSPEC_CONNECTOR_WSPM_FM.WSPM_TinFile.name() ) );
      tinReference.setUpdateDate( date.toGregorianCalendar().getTime() );
      tinReference.setSourceType( SOURCETYPE.gml );
      GmlSerializer.serializeWorkspace( fmOutputFile, fmModel, "UTF-8" ); //$NON-NLS-1$
      setStatus( IStatus.OK, "Success" );
      resultEater.addResult( MODELSPEC_CONNECTOR_WSPM_FM.FM_Model.name(), fmOutputFile );
    }
    catch( final Exception e )
    {
      throw new SimulationException( "Problem bei der Übertragung der Wasserspiegellage von KalypsoWSPM zu KalypsoFlood", e );
    }
  }

  private boolean deleteRunOffEvents( final ISimulationDataProvider inputProvider ) throws SimulationException
  {
    if( inputProvider.hasID( MODELSPEC_CONNECTOR_WSPM_FM.OPT_DeleteExistingRunoffEvents.name() ) )
    {
      final String pathString = inputProvider.getInputForID( MODELSPEC_CONNECTOR_WSPM_FM.OPT_DeleteExistingRunoffEvents.name() ).toString();
      return Boolean.parseBoolean( pathString.substring( pathString.lastIndexOf( "/" ) + 1 ) ); //$NON-NLS-1$
    }

    return false;
  }
}
