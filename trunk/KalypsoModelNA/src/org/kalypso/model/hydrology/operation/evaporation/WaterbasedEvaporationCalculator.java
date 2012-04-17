/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import java.util.Calendar;

import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.timeseries.base.ITimeseriesCache;

/**
 * @author Dirk Kuch
 */
public class WaterbasedEvaporationCalculator extends AbstractEvaporationCalculator
{
  private double m_latitude = 53.64;

  /* Faktor zur Umrechnung von j/cm² in W/m² */
  private double m_factorConversionJw = 8.64; // 24.0 * 60.0 * 60.0 / 10000.0;

  private double m_coefficientEmission = 0.97;

  /* Stefan Boltzmann Konstante für Wasser (DVWK - Formel 5.27) */
  private double m_boltzmannWaterConstant = 0.0000000567; // 5.67 * Math.pow( 10.0, -8.0 );

  /* Albedo der Wasserfläche */
  private double m_albedoWater = 0.05;

  /**
   * @param daterange
   *          calculate evaporation for this date range
   */
  public WaterbasedEvaporationCalculator( final ITimeseriesCache humidity, final ITimeseriesCache sunshine, final ITimeseriesCache temperature, final ITimeseriesCache windVelocity, final DateRange daterange )
  {
    super( humidity, sunshine, temperature, windVelocity, daterange );
  }

  @Override
  protected Double doCalculate( final double humidity, final double sunshine, final double temperature, final double windVelocity, final Calendar date )
  {
    final double es = 6.11 * Math.pow( 10.0, 7.48 * temperature / (237.0 + temperature) );
    final double e = es * humidity / 100.0;
    final double roh = 0.0172 * date.get( Calendar.DAY_OF_YEAR ) - 1.39;
    final double rohSinus = Math.sin( roh );
    final double l = 28.9 - 0.028 * temperature;

    final double r0 = 245.0 * (9.9 + 7.08 * rohSinus + 0.18 * (getLatitude() - 51.0) * (rohSinus - 1)) / getFactorConversionJw();
    final double s0 = 12.3 + rohSinus * (4.3 + (getLatitude() - 51.0) / 6.0);

    final double rg = r0 * (0.19 + 0.55 * sunshine / s0);
    final double rnl = getCoefficientEmission() * getBoltzmannWaterConstant() * Math.pow( temperature + 273.15, 4.0 ) * (0.56 - 0.08 * Math.sqrt( e )) * (0.1 + 0.9 * sunshine / s0);
    final double rn = rg * (1 - getAlbedoWater()) - rnl;

    final double s = es * 4284.0 / Math.pow( 243.12 + temperature, 2.0 );

    final double fv = 0.136 + 0.105 * windVelocity;

    final double ew = (s * rn / l + 0.655 * fv * (es - e)) / (s + 0.655);
    if( ew > 0.0 )
      return ew;

    return 0.0;
  }

  public double getLatitude( )
  {
    return m_latitude;
  }

  public void setLatitude( final double latitude )
  {
    m_latitude = latitude;
  }

  public double getFactorConversionJw( )
  {
    return m_factorConversionJw;
  }

  public void setFactorConversionJw( final double factorConversionJw )
  {
    m_factorConversionJw = factorConversionJw;
  }

  public double getCoefficientEmission( )
  {
    return m_coefficientEmission;
  }

  public void setCoefficientEmission( final double coefficientEmission )
  {
    m_coefficientEmission = coefficientEmission;
  }

  public double getBoltzmannWaterConstant( )
  {
    return m_boltzmannWaterConstant;
  }

  public void setBoltzmannWaterConstant( final double boltzmannWaterConstant )
  {
    m_boltzmannWaterConstant = boltzmannWaterConstant;
  }

  public double getAlbedoWater( )
  {
    return m_albedoWater;
  }

  public void setAlbedoWater( final double albedoWater )
  {
    m_albedoWater = albedoWater;
  }

  @Override
  protected String getParameterType( )
  {
    return ITimeseriesConstants.TYPE_EVAPORATION_WATER_BASED;
  }
}
