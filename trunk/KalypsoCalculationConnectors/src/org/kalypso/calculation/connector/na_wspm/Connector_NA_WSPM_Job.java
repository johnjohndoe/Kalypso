package org.kalypso.calculation.connector.na_wspm;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.calculation.connector.IKalypsoModelConnectorType.MODELSPEC_CONNECTOR_NA_WSPM;
import org.kalypso.calculation.connector.utils.Connectors;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.model.hydrology.binding.model.visitors.FindNodeByNameVisitor;
import org.kalypso.model.hydrology.statistics.INaStatistics;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.IRunOffEvent;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.timeseries.AxisUtils;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.simulation.core.AbstractInternalStatusJob;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

public class Connector_NA_WSPM_Job extends AbstractInternalStatusJob implements ISimulation
{
  public Connector_NA_WSPM_Job( )
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

    final GMLWorkspace workspaceNA = Connectors.getWorkspace( inputProvider, MODELSPEC_CONNECTOR_NA_WSPM.NA_Model.name() );
    if( Objects.isNull( workspaceNA ) )
    {
      setStatus( IStatus.ERROR, "no input file" );
      return;
    }

    final GMLWorkspace workspaceControlModelNA = Connectors.getWorkspace( inputProvider, MODELSPEC_CONNECTOR_NA_WSPM.NA_ControlModel.name() );
    final GMLWorkspace workspaceWSPM = Connectors.getWorkspace( inputProvider, MODELSPEC_CONNECTOR_NA_WSPM.WSPM_Model.name() );

    final URL urlStatisticalReportNA = (URL) inputProvider.getInputForID( MODELSPEC_CONNECTOR_NA_WSPM.NA_StatisticalReport.name() );

    try
    {
      final File outputFile = File.createTempFile( "outTempWspmModel", ".gml", tmpdir ); //$NON-NLS-1$ //$NON-NLS-2$

      try
      {

        final List<BigDecimal> kilometerValuesList = new ArrayList<>();

        final IRunOffEvent runOffEvent = Connectors.findRunOffEvent( workspaceWSPM, inputProvider );
        if( Objects.isNull( runOffEvent ) )
          throw new SimulationException( "Undefined WSPM runoff event ID specified." );

        final org.kalypso.observation.IObservation<TupleResult> runOffEventObservation = ObservationFeatureFactory.toObservation( runOffEvent );
        final TupleResult wspmRunoffEventTupleResult = runOffEventObservation.getResult();
        wspmRunoffEventTupleResult.clear();

        final IComponent[] wspmRunoffEventTupleResultComponents = wspmRunoffEventTupleResult.getComponents();

        final int stationComp = findComponent( wspmRunoffEventTupleResultComponents, IWspmConstants.LENGTH_SECTION_PROPERTY_STATION );
        final int abflussComp = findComponent( wspmRunoffEventTupleResultComponents, IWspmConstants.LENGTH_SECTION_PROPERTY_RUNOFF );
        wspmRunoffEventTupleResult.setSortComponents( new IComponent[] { wspmRunoffEventTupleResultComponents[abflussComp] } );

        final IObservation maxDischargesObservation = ZmlFactory.parseXML( urlStatisticalReportNA );
        final IAxis[] axisList = maxDischargesObservation.getAxes();
        final IAxis axisNodeNr = AxisUtils.findAxisByName( axisList, INaStatistics.AXIS_NODE_ID );
        final IAxis axisMaxDischarge = AxisUtils.findAxisByName( axisList, INaStatistics.AXIS_DISCHARGE );
        if( Objects.isNull( axisNodeNr, axisMaxDischarge ) )
        {
          // TODO error report & exit
          setStatus( IStatus.ERROR, "No proper axis found." );
          return;
        }

        final ITupleModel tuppleModel = maxDischargesObservation.getValues( null );

        final NaModell naModel = (NaModell) workspaceNA.getRootFeature();
        final IFeatureBindingCollection<Node> nodes = naModel.getNodes();

        for( int i = 0; i < tuppleModel.size(); i++ )
        {
          final String nodeNr = tuppleModel.get( i, axisNodeNr ).toString();
          final Double maxDischarge = Double.parseDouble( tuppleModel.get( i, axisMaxDischarge ).toString() );

          final FindNodeByNameVisitor visitor = new FindNodeByNameVisitor( nodeNr );
          nodes.accept( visitor );

          final Node node = visitor.getNode();
          if( Objects.isNotNull( node ) )
          {
// FIXME validate river code

// final Object riverCode = inputProvider.getInputForID( MODELSPEC_CONNECTOR_NA_WSPM.NA_RiverCode.name() );
// final String naRiverCode = riverCode == null ? "" : riverCode.toString();

// if( Strings.isNotEmpty( naRiverCode ) )
// {
// final String nodeRiverCode = node.getRiverCode();
// if( Objects.notEqual( naRiverCode, nodeRiverCode ) )
// continue;
// }

            final Double nodeRiverKilometerProperty = node.getRiverKm();
            if( nodeRiverKilometerProperty != null )
            {
              final BigDecimal bigDecimalRiverKilometer = new BigDecimal( nodeRiverKilometerProperty ).setScale( IProfileFeature.STATION_SCALE, RoundingMode.HALF_UP );
              final BigDecimal bigDecimalDischarge = new BigDecimal( maxDischarge ).setScale( 3, RoundingMode.HALF_UP );

              if( kilometerValuesList.contains( bigDecimalRiverKilometer ) )
              {
                // TODO: what to do if there are
                // duplicate values for river kilometer?
                // It is not allowed as WSPM input!
                setStatus( IStatus.ERROR, "Duplicate value for river kilometer (formatted to 0.0000): " + bigDecimalRiverKilometer );
                return;
              }
              else
              {
                kilometerValuesList.add( bigDecimalRiverKilometer );
              }

              final IRecord record = wspmRunoffEventTupleResult.createRecord();
              wspmRunoffEventTupleResult.add( record );
              record.setValue( stationComp, bigDecimalRiverKilometer );
              record.setValue( abflussComp, bigDecimalDischarge );
            }
          }
        }

        ObservationFeatureFactory.toFeature( runOffEventObservation, runOffEvent );
        final NAControl naControl = (NAControl) workspaceControlModelNA.getRootFeature();

        final Integer returnPeriod = naControl.getReturnPeriod();
        final int returnPeriodToSet = returnPeriod != null && returnPeriod > 0 ? returnPeriod : 1;

        runOffEvent.setProperty( IRunOffEvent.PROPERTY_ANNUALITY, returnPeriodToSet );

        GmlSerializer.serializeWorkspace( outputFile, workspaceWSPM, "UTF-8" ); //$NON-NLS-1$
        setStatus( IStatus.OK, "Success" );
      }
      catch( final Exception e )
      {
        setStatus( IStatus.ERROR, e.getLocalizedMessage() );
      }

      if( isOkStatus() )
      {
        resultEater.addResult( MODELSPEC_CONNECTOR_NA_WSPM.WSPM_Model.name(), outputFile );
      }
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
  }

  private int findComponent( final IComponent[] components, final String name )
  {
    for( int index = 0; index < components.length; index++ )
    {
      final IComponent component = components[index];
      final String identifier = component.getId();

      if( identifier.startsWith( name ) )
        return index;
    }

    return -1;
  }

}
