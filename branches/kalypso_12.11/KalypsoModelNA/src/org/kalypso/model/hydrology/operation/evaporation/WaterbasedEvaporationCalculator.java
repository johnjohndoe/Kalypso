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

import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;

/**
 * @author Dirk Kuch
 */
public class WaterbasedEvaporationCalculator extends AbstractEvaporationCalculator
{
  private double m_latitude = Double.NaN;

  /* Faktor zur Umrechnung von j/cm² in W/m² */
  private final double m_factorConversionJw = 8.64; // 24.0 * 60.0 * 60.0 / 10000.0;

  private final double m_coefficientEmission = 0.97;

  /* Stefan Boltzmann Konstante für Wasser (DVWK - Formel 5.27) */
  private final double m_boltzmannWaterConstant = 0.0000000567; // 5.67 * Math.pow( 10.0, -8.0 );

  /* Albedo der Wasserfläche */
  private final double m_albedoWater = 0.05;

  @Override
  public void init( final ICalculateEvaporationData data ) throws SensorException
  {
    super.init( data );

    m_latitude = data.getLatitude();
  }

  @Override
  public String getLabel( )
  {
    return Messages.getString( "WaterbasedEvaporationCalculator_0" ); //$NON-NLS-1$
  }

  @Override
  public Double doCalculate( final double humidity, final double sunshine, final double temperature, final double windVelocity, final Calendar date )
  {
    final double es = 6.11 * Math.pow( 10.0, 7.48 * temperature / (237.0 + temperature) );
    final double e = es * humidity / 100.0;
    final double roh = 0.0172 * date.get( Calendar.DAY_OF_YEAR ) - 1.39;
    final double rohSinus = Math.sin( roh );
    final double l = 28.9 - 0.028 * temperature;

    final double r0 = 245.0 * (9.9 + 7.08 * rohSinus + 0.18 * (m_latitude - 51.0) * (rohSinus - 1)) / m_factorConversionJw;
    final double s0 = 12.3 + rohSinus * (4.3 + (m_latitude - 51.0) / 6.0);

    final double rg = r0 * (0.19 + 0.55 * sunshine / s0);
    final double rnl = m_coefficientEmission * m_boltzmannWaterConstant * Math.pow( temperature + 273.15, 4.0 ) * (0.56 - 0.08 * Math.sqrt( e )) * (0.1 + 0.9 * sunshine / s0);
    final double rn = rg * (1 - m_albedoWater) - rnl;

    final double s = es * 4284.0 / Math.pow( 243.12 + temperature, 2.0 );

    final double fv = 0.136 + 0.105 * windVelocity;

    final double ew = (s * rn / l + 0.655 * fv * (es - e)) / (s + 0.655);
    if( ew > 0.0 )
      return ew;

    return 0.0;
  }

  @Override
  public String getParameterType( )
  {
    return ITimeseriesConstants.TYPE_EVAPORATION_WATER_BASED;
  }
}