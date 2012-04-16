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
import org.kalypso.ogc.sensor.timeseries.base.ITimeseriesCache;

/**
 * @author Dirk Kuch
 */
public class LandbasedEvaporationCalculator extends AbstractEvaporationCalculator
{
  private static final double LATITUDE_DEGREE = 54.00;

  /* Albedo */
  private static final double ALBEDO = 0.23;

  /* Stefan Boltzmann Konstante (DVWK - Formel 5.27) */
// private static final double BOLTZMANN_CONSTANT = 0.49 * Math.pow( 10.0, -6.0 );

  public LandbasedEvaporationCalculator( final ITimeseriesCache humidity, final ITimeseriesCache sunshine, final ITimeseriesCache temperature, final ITimeseriesCache windVelocity, final DateRange daterange )
  {
    super( humidity, sunshine, temperature, windVelocity, daterange );
  }

  @Override
  protected Double doCalculate( final Double humidity, final Double sunshine, final Double temperature, final Double windVelocity, final Calendar date )
  {
    final double roh = 0.0172 * date.get( Calendar.DAY_OF_YEAR ) - 1.39;
    final double r0 = 245.0 * (9.9 + 7.08 * Math.sin( roh ) + 0.18 * (LATITUDE_DEGREE - 51.0) * (Math.sin( roh ) - 1));
    final double s0 = 12.3 + Math.sin( roh ) * (4.3 + (LATITUDE_DEGREE - 51.0) / 6.0);
    final double rg = r0 * (0.19 + 0.55 * sunshine / s0);

    final double es = 6.11 * Math.pow( 10.0, 7.48 * temperature / (237.0 + temperature) );
    final double e = es * (humidity / 100.0);

    final double s = es * (4284.0 / Math.pow( 243.12 + temperature, 2.0 ));

// final double rn = (1.0 - ALBEDO) * rg - BOLTZMANN_CONSTANT * Math.pow( 237.15 + temperature, 4.0 ) * (0.1 + 0.9
// * (sunshine / s0)) * (0.34 - 0.044 * Math.pow( e, 0.5 ));

    final double l = 249.8 - 0.242 * temperature;

    final double v2 = getV2( windVelocity );

    final double etN = s * (0.6 * rg / l) + 0.655 * (3.75 / (temperature + 273.0)) * v2 * (es - e);
    final double etT = s + 0.655 * (1 + 0.34 * v2);

    final double et0 = etN / etT;

    if( et0 > 0.0 )
      return et0;

    return 0.0;
  }

  /**
   * Beaufort-Conversion of wind velocity
   */
  private double getV2( final Double v )
  {
    final double beaufort = 0.0009 * Math.pow( v, 6.0 ) - 0.0173 * Math.pow( v, 5.0 ) + 0.1219 * Math.pow( v, 4.0 ) - 0.3874 * Math.pow( v, 3.0 ) + 0.8597 * Math.pow( v, 2.0 ) + 0.2767 * v - 0.0186;

    return beaufort;
  }
}
