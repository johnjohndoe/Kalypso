package org.kalypso.model.wspm.sobek.result.processing.worker;

import java.math.BigDecimal;
import java.util.Calendar;
import java.util.Comparator;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.Map.Entry;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.Duration;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;

import nl.wldelft.fews.pi.EventComplexType;
import nl.wldelft.fews.pi.TimeSerieComplexType;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.sobek.core.interfaces.INode;
import org.kalypso.model.wspm.sobek.core.utils.AtomarAddFeatureCommand;
import org.kalypso.model.wspm.sobek.result.processing.model.IResultTimeSeries;
import org.kalypso.model.wspm.sobek.result.processing.model.IValuePairMember;
import org.kalypso.model.wspm.sobek.result.processing.model.implementation.ResultTimeSeriesHandler;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.FeatureUtils;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;

import com.sun.org.apache.xerces.internal.jaxp.datatype.XMLGregorianCalendarImpl;

public class ResultWorker
{
  private final ResultTimeSeriesHandler m_handler;

  private final TimeSerieComplexType m_binding;

  private double m_max = Double.MIN_VALUE;

  private double m_min = Double.MAX_VALUE;

  private double m_last = Double.NaN;

  /* result table <Waterlevel, Number of intervals> */
  private final Map<Double, Integer> m_resultTable = new TreeMap<Double, Integer>();

  private final CommandableWorkspace m_workspace;

  private final INode m_node;

  public ResultWorker( final CommandableWorkspace targetWorkspace, final TimeSerieComplexType binding, final INode node )
  {
    m_workspace = targetWorkspace;
    m_binding = binding;
    m_node = node;
    m_handler = new ResultTimeSeriesHandler( targetWorkspace.getRootFeature(), m_node );
  }

  public void process( IResultWorkerSettings settings ) throws CoreException
  {
    final IObservation<TupleResult> observation = m_handler.getObservation();

    /* read out time series binding and fill observation */
    final TupleResult result = observation.getResult();

    final List<EventComplexType> events = m_binding.getEvent();
    for( final EventComplexType event : events )
    {
      /* data */
      final XMLGregorianCalendar date = event.getDate();
      final XMLGregorianCalendar time = event.getTime();

      final GregorianCalendar calendar = new GregorianCalendar();
      calendar.set( Calendar.DAY_OF_MONTH, date.getDay() );
      calendar.set( Calendar.MONTH, date.getMonth() );
      calendar.set( Calendar.YEAR, date.getYear() );
      calendar.set( Calendar.HOUR_OF_DAY, time.getHour() );
      calendar.set( Calendar.MINUTE, time.getMinute() );
      calendar.set( Calendar.SECOND, time.getSecond() );

      final Double value = event.getValue();

      /* statistic results */
      determineMinMaxLast( value );

      addResultTableValue( value );

      /* new record */
      final IRecord record = result.createRecord();
      record.setValue( 0, new XMLGregorianCalendarImpl( calendar ) );
      record.setValue( 1, BigDecimal.valueOf( value ) );

      result.add( record );
    }

    final Object property = m_handler.getProperty( IResultTimeSeries.QN_RESULT_MEMBER );
    if( property instanceof Feature )
      ObservationFeatureFactory.toFeature( observation, (Feature) property );

    try
    {
      /* intervall of timeseries */
      final int intervall = ResultHelper.getIntervallAsSeconds( m_binding.getHeader() );
      final Map<Duration, Double> results = getResultTable( intervall );
      final Set<Entry<Duration, Double>> entrySet = results.entrySet();
      for( final Entry<Duration, Double> entry : entrySet )
      {
        addValuePair( entry.getKey(), entry.getValue() );
      }

      /* update properties */
      final Map<QName, Object> properties = new HashMap<QName, Object>();

      properties.put( IResultTimeSeries.QN_UNIQUE_ID, m_node.getId() ); // uniqueId
      properties.put( IResultTimeSeries.QN_STATION_NAME, m_node.getStationName() ); // station name
      properties.put( IResultTimeSeries.QN_NAME, m_node.getName() ); // name
      properties.put( IResultTimeSeries.QN_PARAM_ID, settings.getParameterId() ); // parameter id
// properties.put( IResultTimeSeries.QN_PARAM_ID, "W" ); // parameter id

      properties.put( IResultTimeSeries.QN_UNIT, settings.getUnit() ); // unit
// properties.put( IResultTimeSeries.QN_UNIT, "m NHN" ); // unit
      properties.put( IResultTimeSeries.QN_MAX_VALUE, m_max );
      properties.put( IResultTimeSeries.QN_MIN_VALUE, m_min );
      properties.put( IResultTimeSeries.QN_LAST_VALUE, m_last );

      FeatureUtils.updateProperties( m_workspace, m_handler, properties );
    }
    catch( final Exception e )
    {
      throw new CoreException( StatusUtilities.createErrorStatus( e.getMessage() ) );
    }
  }

  private void addValuePair( final Duration duration, final Double value ) throws Exception
  {
    final IGMLSchema schema = m_handler.getFeatureType().getGMLSchema();
    final IFeatureType featureType = schema.getFeatureType( IValuePairMember.QN_TYPE );

    final IRelationType rt = (IRelationType) m_handler.getFeatureType().getProperty( IResultTimeSeries.QN_VALUE_PAIRS );

    final IFeatureSelectionManager selectionManager = KalypsoCorePlugin.getDefault().getSelectionManager();

    final Map<IPropertyType, Object> values = new HashMap<IPropertyType, Object>();
    values.put( featureType.getProperty( IValuePairMember.QN_TIME_PERIODE ), duration );
    values.put( featureType.getProperty( IValuePairMember.QN_WATERLEVEL ), value );

    final AtomarAddFeatureCommand command = new AtomarAddFeatureCommand( m_workspace, featureType, m_handler, rt, -1, values, selectionManager );
    m_workspace.postCommand( command );
  }

  private Map<Duration, Double> getResultTable( final int intervall ) throws DatatypeConfigurationException
  {
    final DatatypeFactory factory = DatatypeFactory.newInstance();

    final Map<Duration, Double> myResult = new TreeMap<Duration, Double>( new Comparator<Duration>()
    {
      public int compare( Duration o1, Duration o2 )
      {
        return o1.compare( o2 );
      }
    } );

    final Object[] values = m_resultTable.entrySet().toArray();

    int duration = 0;

    for( int i = values.length - 1; i >= 0; i-- )
    {
      final Entry<Double, Integer> entry = (Entry<Double, Integer>) values[i];

      final Integer occurrence = entry.getValue();
      final Double waterlevel = entry.getKey();

      duration += occurrence * intervall;

      final Duration d = factory.newDuration( true, 0, 0, 0, 0, 0, duration );
      myResult.put( d, waterlevel );
    }

    return myResult;
  }

  private void addResultTableValue( final Double value )
  {
    // Normalise value to precision .4f
    final Double myValue = Double.valueOf( value * 10000 ).intValue() / 10000d;

    Integer integer = m_resultTable.get( myValue );
    if( integer == null )
    {
      integer = 0;
    }

    m_resultTable.put( myValue, ++integer );
  }

  private void determineMinMaxLast( final Double value )
  {
    m_min = Math.min( m_min, value );
    m_max = Math.max( m_max, value );
    m_last = value;
  }

}
