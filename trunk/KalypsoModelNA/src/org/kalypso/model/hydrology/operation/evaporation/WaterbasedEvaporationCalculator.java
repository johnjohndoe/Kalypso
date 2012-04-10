/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
import java.util.SortedMap;
import java.util.TreeMap;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.java.lang.Doubles;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.java.util.CalendarUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.TupleModelDataSet;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.timeseries.base.ITimeseriesCache;

/**
 * @author Dirk Kuch
 */
public class WaterbasedEvaporationCalculator implements ICoreRunnableWithProgress
{
  private final ITimeseriesCache m_humidity;

  private final ITimeseriesCache m_sunshine;

  private final ITimeseriesCache m_temperature;

  private final ITimeseriesCache m_windVelocity;

  private final DateRange m_daterange;

  /**
   * @param daterange
   *          calculate evaporation for this date range
   */
  public WaterbasedEvaporationCalculator( final ITimeseriesCache humidity, final ITimeseriesCache sunshine, final ITimeseriesCache temperature, final ITimeseriesCache windVelocity, final DateRange daterange )
  {
    m_humidity = humidity;
    m_sunshine = sunshine;
    m_temperature = temperature;
    m_windVelocity = windVelocity;

    m_daterange = daterange;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    monitor.setTaskName( "Starting calcualtion of water based evaporation." );

    final StatusCollector stati = new StatusCollector( ModelNA.PLUGIN_ID );

    final Calendar to = CalendarUtilities.getCalendar( m_daterange.getTo(), KalypsoCorePlugin.getDefault().getTimeZone() );
    final Calendar ptr = CalendarUtilities.getCalendar( m_daterange.getFrom(), KalypsoCorePlugin.getDefault().getTimeZone() );

    // iterate over values inherit the given date range
    while( ptr.before( to ) || ptr.equals( to ) )
    {
      final Double humidity = getValue( getDataSet( m_humidity, ptr, ITimeseriesConstants.TYPE_MEAN_HUMIDITY ) );
      final Double sunshine = getValue( getDataSet( m_sunshine, ptr, ITimeseriesConstants.TYPE_MEAN_SUNSHINE_HOURS ) );
      final Double temperature = getValue( getDataSet( m_temperature, ptr, ITimeseriesConstants.TYPE_MEAN_TEMPERATURE ) );
      final Double windVelocity = getValue( getDataSet( m_windVelocity, ptr, ITimeseriesConstants.TYPE_MEAN_WIND_VELOCITY ) );

      if( Doubles.isNaN( humidity, sunshine, temperature, windVelocity ) )
      {
        final SimpleDateFormat sdf = new SimpleDateFormat( "dd.MM.yyyy" ); //$NON-NLS-1$
        sdf.setTimeZone( KalypsoCorePlugin.getDefault().getTimeZone() );

        final String msg = String.format( "Can't calucate evaporation for date %s. Missing parameter: humidity: %.2f, sunshine: %.2f, temperature: %.2f, wind velocity: %.2f", //
            sdf.format( ptr.getTime() ), //
            Objects.firstNonNull( humidity, Double.NaN ), //
            Objects.firstNonNull( sunshine, Double.NaN ), //
            Objects.firstNonNull( temperature, Double.NaN ), //
            Objects.firstNonNull( windVelocity, Double.NaN ) );

        stati.add( IStatus.WARNING, msg );
      }

      final Double evaporation = doCalculate( humidity, sunshine, temperature, windVelocity, ptr );

      ptr.add( Calendar.HOUR_OF_DAY, 24 );
    }

    monitor.done();

    return stati.asMultiStatus( "Water based evaporation calculation" );
  }

  private static final double LATITUDE_DEGREE = 53.64;

  /* Faktor zur Umrechnung von j/cm� in W/m� */
  private static final double FACTOR_CONVERSION_JW = 24.0 * 60.0 * 60.0 / 10000.0;

  private static final double COEFFICIENT_EMISSION = 0.97;

  /* Stefan Boltzmann Konstante f�r Wasser (DVWK - Formel 5.27) */
  private static final double BOLTZMANN_WATER_CONSTANT = 5.67 * Math.pow( 10.0, -8.0 );

  /* Albedo der Wasserfl�che */
  private static final double ALBEDO = 0.05;

  private Double doCalculate( final Double humidity, final Double sunshine, final Double temperature, final Double windVelocity, final Calendar date )
  {
    final double es = 6.11 * Math.pow( 10.0, 7.48 * temperature / (237.0 + temperature) );
    final double e = es * humidity / 100.0;
    final double roh = 0.0172 * date.get( Calendar.DAY_OF_YEAR ) - 1.39;
    final double l = 28.9 - 0.028 * temperature;

    final double r0 = 245.0 * (9.9 + 7.08 * Math.sin( roh ) + 0.18 * (LATITUDE_DEGREE - 51.0) * (Math.sin( roh ) - 1)) / FACTOR_CONVERSION_JW;
    final double s0 = 12.3 + Math.sin( roh ) * (4.3 + (LATITUDE_DEGREE - 51.0) / 6.0);

    final double rg = r0 * (0.19 + 0.55 * sunshine / s0);
    final double rnl = COEFFICIENT_EMISSION * BOLTZMANN_WATER_CONSTANT * Math.pow( temperature + 273.15, 4.0 ) * (0.56 - 0.08 * Math.sqrt( e )) * (0.1 + 0.9 * sunshine / s0);
    final double rn = rg * (1 - ALBEDO) - rnl;

    final double s = es * 4284.0 / Math.pow( 243.12 + temperature, 2.0 );

    final double fv = 0.136 + 0.105 * windVelocity;

    final double ew = (s * rn / l + 0.655 * fv * (es - e)) / (s + 0.655);

    if( ew > 0.0 )
      return ew;

    return 0.0;
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
}
