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

import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.j3d.geom.UnsupportedTypeException;
import org.kalypso.commons.java.lang.Arrays;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.timeseries.AxisUtils;
import org.kalypso.ogc.sensor.util.DateRanges;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.view.TimeseriesBean;

/**
 * @author Dirk Kuch
 */
public class CalculateEvaporationData extends AbstractModelObject
{
  public enum EVAPORATION_TYPE
  {
    eLandBased,
    eWaterBase;

    @Override
    public String toString( )
    {
      final EVAPORATION_TYPE type = valueOf( name() );
      switch( type )
      {
        case eLandBased:
          return Messages.getString( "CalculateEvaporationData_0" ); //$NON-NLS-1$
        case eWaterBase:
          return Messages.getString( "CalculateEvaporationData_1" ); //$NON-NLS-1$

        default:
          throw new UnsupportedTypeException();
      }

    }

    public boolean isTypeOf( final String parameterType )
    {
      final EVAPORATION_TYPE type = valueOf( name() );
      switch( type )
      {
        case eLandBased:
          return StringUtils.equalsIgnoreCase( ITimeseriesConstants.TYPE_EVAPORATION_LAND_BASED, parameterType );

        case eWaterBase:
          return StringUtils.equalsIgnoreCase( ITimeseriesConstants.TYPE_EVAPORATION_WATER_BASED, parameterType );
      }

      return false;
    }
  }

  public static final String PROPERTY_HUMIDITY = "humidity"; //$NON-NLS-1$

  public static final String PROPERTY_TEMPERATURE = "temperature"; //$NON-NLS-1$

  public static final String PROPERTY_WIND_VELOCITY = "windVelocity"; //$NON-NLS-1$

  public static final String PROPERTY_SUNSHINE_HOURS = "sunshineHours"; //$NON-NLS-1$

  public static final String PROPERTY_QUALITY = "quality"; //$NON-NLS-1$

  public static final String PROPERTY_EVAPORATION_TYPE = "evaporationType"; //$NON-NLS-1$

  public static final String PROPERTY_LATITUDE = "latitude"; //$NON-NLS-1$

  public static final String PROPERTY_FACTOR_CONVERSION_JW = "factorConversionJw"; //$NON-NLS-1$

  public static final String PROPERTY_COEFFICIENT_EMISSION = "coefficientEmission"; //$NON-NLS-1$

  public static final String PROPERTY_BOLTZMANN_WATER_CONSTANT = "boltzmannWaterConstant"; //$NON-NLS-1$

  public static final String PROPERTY_ALBEDO_WATER = "albedoWater"; //$NON-NLS-1$

  private TimeseriesBean m_humidity;

  private TimeseriesBean m_temperature;

  private TimeseriesBean m_windVelocity;

  private TimeseriesBean m_sunshineHours;

  private String m_quality;

  private EVAPORATION_TYPE m_evaporationType;

  private String m_latitude = "53,64"; //$NON-NLS-1$

  private String m_factorConversionJw = "8,64"; //$NON-NLS-1$

  private String m_coefficientEmission = "0,97"; //$NON-NLS-1$

  private String m_boltzmannWaterConstant = "0,0000000567"; //$NON-NLS-1$

  private String m_albedoWater = "0.05"; //$NON-NLS-1$

  public void init( final IDialogSettings settings )
  {
    // TODO Auto-generated method stub
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

  public String getQuality( )
  {
    return m_quality;
  }

  public void setQuality( final String quality )
  {
    m_quality = quality;
  }

  public EVAPORATION_TYPE getEvaporationType( )
  {
    return m_evaporationType;
  }

  public void setEvaporationType( final EVAPORATION_TYPE evaporationType )
  {
    m_evaporationType = evaporationType;
  }

  public String getLatitude( )
  {
    return m_latitude;
  }

  public void setLatitude( final String latitude )
  {
    m_latitude = latitude;
  }

  public String getFactorConversionJw( )
  {
    return m_factorConversionJw;
  }

  public void setFactorConversionJw( final String factorConversionJw )
  {
    m_factorConversionJw = factorConversionJw;
  }

  public String getCoefficientEmission( )
  {
    return m_coefficientEmission;
  }

  public void setCoefficientEmission( final String coefficientEmission )
  {
    m_coefficientEmission = coefficientEmission;
  }

  public String getBoltzmannWaterConstant( )
  {
    return m_boltzmannWaterConstant;
  }

  public void setBoltzmannWaterConstant( final String boltzmannWaterConstant )
  {
    m_boltzmannWaterConstant = boltzmannWaterConstant;
  }

  public String getAlbedoWater( )
  {
    return m_albedoWater;
  }

  public void setAlbedoWater( final String albedoWater )
  {
    m_albedoWater = albedoWater;
  }

}
