/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.hydrology.operation.evaporation;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.SortedMap;
import java.util.TimeZone;
import java.util.TreeMap;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.joda.time.LocalTime;
import org.kalypso.commons.java.lang.Doubles;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.java.util.CalendarUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.TupleModelDataSet;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTupleModel;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.metadata.MetadataHelper;
import org.kalypso.ogc.sensor.metadata.MetadataList;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.timeseries.TimeseriesUtils;
import org.kalypso.ogc.sensor.timeseries.base.ITimeseriesCache;
import org.kalypso.ogc.sensor.timeseries.datasource.DataSourceHandler;
import org.kalypso.ogc.sensor.timeseries.datasource.DataSourceHelper;
import org.kalypso.repository.IDataSourceItem;

/**
 * @author Dirk Kuch
 */
public abstract class AbstractEvaporationCalculator implements IEvaporationCalculator
{
  public static final String DATA_SOURCE = IDataSourceItem.SOURCE_PREFIX + "evaporation.calculation"; //$NON-NLS-1$

  private ITimeseriesCache m_humidity;

  private ITimeseriesCache m_sunshine;

  private ITimeseriesCache m_temperature;

  private ITimeseriesCache m_windVelocity;

  private DateRange m_daterange;

  private final Map<Date, Double> m_results = new TreeMap<>();

  private IObservation m_observation;

  @Override
  public void init( final ICalculateEvaporationData data ) throws SensorException
  {
    m_humidity = data.getHumidityData();
    m_sunshine = data.getSunshineData();
    m_temperature = data.getTemperatureData();
    m_windVelocity = data.getWindVelocityData();
    m_daterange = data.getDateRange();
  }

  @Override
  public final String toString( )
  {
    final String paramerLabel = TimeseriesUtils.getName( getParameterType() );

    return String.format( "%s: %s", paramerLabel, getLabel() ); //$NON-NLS-1$
  }

  protected abstract String getLabel( );

  private ITimeseriesCache getHumidity( )
  {
    return m_humidity;
  }

  private ITimeseriesCache getSunshine( )
  {
    return m_sunshine;
  }

  private ITimeseriesCache getTemperature( )
  {
    return m_temperature;
  }

  private ITimeseriesCache getWindVelocity( )
  {
    return m_windVelocity;
  }

  private DateRange getDateRange( )
  {
    return m_daterange;
  }

  private void addResult( final Date time, final Double evaporation )
  {
    m_results.put( time, evaporation );
  }

  private Double getValue( final TupleModelDataSet dataSet )
  {
    if( Objects.isNull( dataSet ) )
      return null;

    final Object value = dataSet.getValue();
    if( !(value instanceof Number) )
      return Double.NaN;

    return ((Number) value).doubleValue();
  }

  private TupleModelDataSet getDataSet( final ITimeseriesCache humidity, final Calendar ptr, final String type )
  {
    final TreeMap<Date, TupleModelDataSet[]> valueMap = humidity.getValueMap();

    final Date base = ptr.getTime();

    final SortedMap<Date, TupleModelDataSet[]> headMap = valueMap.headMap( base );
    final SortedMap<Date, TupleModelDataSet[]> tailMap = valueMap.tailMap( base );

    if( !headMap.isEmpty() && DateUtils.isSameDay( headMap.lastKey(), base ) )
      return getDataSet( headMap.get( headMap.lastKey() ), type );
    else if( !tailMap.isEmpty() && DateUtils.isSameDay( tailMap.firstKey(), base ) )
      return getDataSet( tailMap.get( tailMap.firstKey() ), type );

    return null;
  }

  private TupleModelDataSet getDataSet( final TupleModelDataSet[] sets, final String type )
  {
    if( ArrayUtils.isEmpty( sets ) )
      return null;
    else if( ArrayUtils.getLength( sets ) == 1 )
      return sets[0];

    for( final TupleModelDataSet set : sets )
    {
      if( StringUtils.equals( set.getValueAxis().getType(), type ) )
        return set;
    }

    return null;
  }

  private IObservation buildObservation( )
  {
    final IAxis dateAxis = TimeseriesUtils.createDefaultAxis( ITimeseriesConstants.TYPE_DATE );
    final IAxis valueAxis = TimeseriesUtils.createDefaultAxis( getParameterType() );
    final IAxis statusAxis = KalypsoStatusUtils.createStatusAxisFor( valueAxis, true );
    final IAxis dataSourceAxis = DataSourceHelper.createSourceAxis( valueAxis, true );

    /* create meta data */
    final MetadataList metadata = new MetadataList();
    final DataSourceHandler sources = new DataSourceHandler( metadata );
    final Integer source = sources.addDataSource( DATA_SOURCE, DATA_SOURCE );
    final Integer status = KalypsoStati.BIT_OK;

    /* Timestep and timstamp are hardcoded into the algorithm: 1 day and 12:00 */
    MetadataHelper.setTimestep( metadata, RESULT_TIMESTEP );
    MetadataHelper.setTimestamp( metadata, getTimeStamp() );

    final SimpleTupleModel model = new SimpleTupleModel( new IAxis[] { dateAxis, valueAxis, statusAxis, dataSourceAxis } );

    final Set<Entry<Date, Double>> values = m_results.entrySet();
    for( final Entry<Date, Double> entry : values )
    {
      final Date date = entry.getKey();
      final Double evaporation = entry.getValue();

      model.addTuple( new Object[] { date, evaporation, status, source } );
    }

    return new SimpleObservation( DATA_SOURCE, DATA_SOURCE, metadata, model );
  }

  private LocalTime getTimeStamp( )
  {
    final TimeZone timeZone = KalypsoCorePlugin.getDefault().getTimeZone();

    final int rawOffset = timeZone.getRawOffset();

    final Calendar calendar = CalendarUtilities.getCalendar( getDateRange().getFrom(), timeZone );
    calendar.set( Calendar.HOUR_OF_DAY, 12 );
    calendar.set( Calendar.MINUTE, 0 );
    calendar.set( Calendar.SECOND, 0 );
    calendar.set( Calendar.MILLISECOND, 0 );

    calendar.add( Calendar.MILLISECOND, -rawOffset );

    /* REMARK: The ISO Chronolgy used will have the UTC timezone set. */

    /* We set the timestamp so, that in the local time, it is always 12:00 */

    return new LocalTime( calendar.get( Calendar.HOUR_OF_DAY ), calendar.get( Calendar.MINUTE ) );
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    monitor.setTaskName( Messages.getString( "AbstractEvaporationCalculator_1" ) ); //$NON-NLS-1$

    final IStatus calcStatus = calculateEvaporation( monitor );

    m_observation = buildObservation();

    return calcStatus;
  }

  private IStatus calculateEvaporation( final IProgressMonitor monitor )
  {
    final IStatusCollector stati = new StatusCollector( ModelNA.PLUGIN_ID );

    final Calendar to = CalendarUtilities.getCalendar( getDateRange().getTo(), KalypsoCorePlugin.getDefault().getTimeZone() );
    final Calendar ptr = CalendarUtilities.getCalendar( getDateRange().getFrom(), KalypsoCorePlugin.getDefault().getTimeZone() );
    ptr.set( Calendar.HOUR_OF_DAY, 12 );
    ptr.set( Calendar.MINUTE, 0 );
    ptr.set( Calendar.SECOND, 0 );
    ptr.set( Calendar.MILLISECOND, 0 );

    // iterate over values inherit the given date range
    while( ptr.before( to ) || DateUtils.isSameDay( ptr, to ) )
    {
      final Double humidity = getValue( getDataSet( getHumidity(), ptr, ITimeseriesConstants.TYPE_MEAN_HUMIDITY ) );
      final Double sunshine = getValue( getDataSet( getSunshine(), ptr, ITimeseriesConstants.TYPE_SUNSHINE_HOURS ) );
      final Double temperature = getValue( getDataSet( getTemperature(), ptr, ITimeseriesConstants.TYPE_MEAN_TEMPERATURE ) );
      final Double windVelocity = getValue( getDataSet( getWindVelocity(), ptr, ITimeseriesConstants.TYPE_MEAN_WIND_VELOCITY ) );

      if( Doubles.isNaN( humidity, sunshine, temperature, windVelocity ) )
      {
        final SimpleDateFormat sdf = new SimpleDateFormat( "dd.MM.yyyy" ); //$NON-NLS-1$
        sdf.setTimeZone( KalypsoCorePlugin.getDefault().getTimeZone() );

        final String msg = String.format( Messages.getString( "AbstractEvaporationCalculator_2" ), // //$NON-NLS-1$
            sdf.format( ptr.getTime() ), //
            Objects.firstNonNull( humidity, Double.NaN ), //
            Objects.firstNonNull( sunshine, Double.NaN ), //
            Objects.firstNonNull( temperature, Double.NaN ), //
            Objects.firstNonNull( windVelocity, Double.NaN ) );

        stati.add( IStatus.WARNING, msg );
      }
      else
      {
        final Double evaporation = doCalculate( humidity, sunshine, temperature, windVelocity, ptr );
        if( Objects.isNotNull( evaporation ) )
          addResult( ptr.getTime(), evaporation );
      }

      ptr.add( Calendar.DAY_OF_MONTH, 1 );
    }

    monitor.done();

    return stati.asMultiStatus( Messages.getString( "AbstractEvaporationCalculator_3" ) ); //$NON-NLS-1$
  }

  protected abstract Double doCalculate( double humidity, double sunshine, double temperature, double windVelocity, Calendar ptr );

  @Override
  public IObservation getObservation( )
  {
    return m_observation;
  }
}