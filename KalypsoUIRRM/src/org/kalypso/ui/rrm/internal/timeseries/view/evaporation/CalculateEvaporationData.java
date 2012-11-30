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
package org.kalypso.ui.rrm.internal.timeseries.view.evaporation;

import java.util.Date;

import org.kalypso.commons.java.lang.Arrays;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.model.hydrology.operation.evaporation.FAOLandbasedEvaporationCalculator;
import org.kalypso.model.hydrology.operation.evaporation.ICalculateEvaporationData;
import org.kalypso.model.hydrology.operation.evaporation.IEvaporationCalculator;
import org.kalypso.model.hydrology.operation.evaporation.WaterbasedEvaporationCalculator;
import org.kalypso.model.hydrology.operation.evaporation.WendlingLandbasedEvaporationCalculator;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.timeseries.AxisUtils;
import org.kalypso.ogc.sensor.timeseries.base.CacheTimeSeriesVisitor;
import org.kalypso.ogc.sensor.timeseries.base.ITimeseriesCache;
import org.kalypso.ogc.sensor.util.DateRanges;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.internal.timeseries.view.TimeseriesBean;

/**
 * @author Dirk Kuch
 */
public class CalculateEvaporationData extends AbstractModelObject implements ICalculateEvaporationData
{
  public static final String PROPERTY_HUMIDITY = "humidity"; //$NON-NLS-1$

  public static final String PROPERTY_TEMPERATURE = "temperature"; //$NON-NLS-1$

  public static final String PROPERTY_WIND_VELOCITY = "windVelocity"; //$NON-NLS-1$

  public static final String PROPERTY_SUNSHINE_HOURS = "sunshineHours"; //$NON-NLS-1$

  public static final String PROPERTY_QUALITY = "quality"; //$NON-NLS-1$

  public static final String PROPERTY_CALCULATOR = "calculator"; //$NON-NLS-1$

  public static final String PROPERTY_LATITUDE = "latitude"; //$NON-NLS-1$

  private TimeseriesBean m_humidity;

  private TimeseriesBean m_temperature;

  private TimeseriesBean m_windVelocity;

  private TimeseriesBean m_sunshineHours;

  private String m_quality;

  private double m_latitude = 53.64;

  private final IEvaporationCalculator[] m_allCalculators = createCalculators();

  private IEvaporationCalculator m_calculator = m_allCalculators[0];

  private final IStation m_station;

  public CalculateEvaporationData( final IStation station )
  {
    m_station = station;
  }

  private static IEvaporationCalculator[] createCalculators( )
  {
    final WendlingLandbasedEvaporationCalculator landWendling = new WendlingLandbasedEvaporationCalculator();
    final FAOLandbasedEvaporationCalculator landFAO = new FAOLandbasedEvaporationCalculator();
    final WaterbasedEvaporationCalculator water = new WaterbasedEvaporationCalculator();

    return new IEvaporationCalculator[] { landWendling, landFAO, water };
  }

  public TimeseriesBean getHumidity( )
  {
    return m_humidity;
  }

  public void setHumidity( final TimeseriesBean humidity )
  {
    m_humidity = humidity;
  }

  public TimeseriesBean getTemperature( )
  {
    return m_temperature;
  }

  public void setTemperature( final TimeseriesBean temperature )
  {
    m_temperature = temperature;
  }

  public TimeseriesBean getWindVelocity( )
  {
    return m_windVelocity;
  }

  public void setWindVelocity( final TimeseriesBean windVelocity )
  {
    m_windVelocity = windVelocity;
  }

  public TimeseriesBean getSunshineHours( )
  {
    return m_sunshineHours;
  }

  public void setSunshineHours( final TimeseriesBean sunshineHours )
  {
    m_sunshineHours = sunshineHours;
  }

  @Override
  public DateRange getDateRange( )
  {
    return getDateRange( toObservation( getHumidity() ), toObservation( getSunshineHours() ), toObservation( getTemperature() ), toObservation( getWindVelocity() ) );
  }

  public IObservation toObservation( final TimeseriesBean bean )
  {
    if( Objects.isNull( bean ) )
      return null;

    final ITimeseries timeseries = bean.getFeature();
    if( Objects.isNull( timeseries ) )
      return null;

    final ZmlLink link = timeseries.getDataLink();

    return link.getObservationFromPool();
  }

  private DateRange getDateRange( final IObservation... observations )
  {
    if( Arrays.isEmpty( observations ) )
      return null;

    DateRange result = null;

    for( final IObservation observation : observations )
    {
      try
      {
        final ITupleModel model = observation.getValues( null );
        if( model.isEmpty() )
          return null;

        final IAxis date = AxisUtils.findDateAxis( model.getAxes() );
        final Date d1 = (Date) model.get( 0, date );
        final Date d2 = (Date) model.get( model.size() - 1, date );

        final DateRange range = new DateRange( d1, d2 );
        result = DateRanges.intersect( result, range );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }

    return result;
  }

  @Override
  public String getQuality( )
  {
    return m_quality;
  }

  public void setQuality( final String quality )
  {
    final String oldQuality = m_quality;

    m_quality = quality;

    firePropertyChange( PROPERTY_QUALITY, oldQuality, m_quality );
  }

  public IEvaporationCalculator getCalculator( )
  {
    return m_calculator;
  }

  public void setCalculator( final IEvaporationCalculator evaporationCalculator )
  {
    final IEvaporationCalculator oldCalculator = getCalculator();

    m_calculator = evaporationCalculator;

    firePropertyChange( PROPERTY_CALCULATOR, oldCalculator, evaporationCalculator );

    // REMARK: fire a change here, so quality validators are reevaluated
    final String oldQuality = getQuality();

    setQuality( oldQuality + "oncechangedinordertowork" ); //$NON-NLS-1$
    setQuality( oldQuality );
  }

  @Override
  public double getLatitude( )
  {
    return m_latitude;
  }

  public void setLatitude( final double latitude )
  {
    m_latitude = latitude;
  }

  @Override
  public ITimeseriesCache getTemperatureData( ) throws SensorException
  {
    return CacheTimeSeriesVisitor.cache( toObservation( m_temperature ) );
  }

  @Override
  public ITimeseriesCache getHumidityData( ) throws SensorException
  {
    return CacheTimeSeriesVisitor.cache( toObservation( m_humidity ) );
  }

  @Override
  public ITimeseriesCache getSunshineData( ) throws SensorException
  {
    return CacheTimeSeriesVisitor.cache( toObservation( m_sunshineHours ) );
  }

  @Override
  public ITimeseriesCache getWindVelocityData( ) throws SensorException
  {
    return CacheTimeSeriesVisitor.cache( toObservation( m_windVelocity ) );
  }

  public IEvaporationCalculator[] getAllCalculators( )
  {
    return m_allCalculators;
  }

  @Override
  public IStation getStation( )
  {
    return m_station;
  }
}