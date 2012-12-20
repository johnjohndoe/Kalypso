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
package org.kalypso.model.hydrology.operation.evaporation.test;

import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.operation.evaporation.ICalculateEvaporationData;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.timeseries.base.ITimeseriesCache;

/**
 * @author Gernot Belger
 */
class DummyCalculateEvaporationData implements ICalculateEvaporationData
{
  private final double m_latitude = 53.64;

  private final ITimeseriesCache m_humidity;

  private final ITimeseriesCache m_sunshine;

  private final ITimeseriesCache m_temperature;

  private final ITimeseriesCache m_windVelocity;

  private final DateRange m_daterange;

  public DummyCalculateEvaporationData( final ITimeseriesCache humidity, final ITimeseriesCache sunshine, final ITimeseriesCache temperature, final ITimeseriesCache windVelocity, final DateRange daterange )
  {
    m_humidity = humidity;
    m_sunshine = sunshine;
    m_temperature = temperature;
    m_windVelocity = windVelocity;
    m_daterange = daterange;
  }

  @Override
  public ITimeseriesCache getTemperatureData( )
  {
    return m_temperature;
  }

  @Override
  public ITimeseriesCache getHumidityData( )
  {
    return m_humidity;
  }

  @Override
  public ITimeseriesCache getSunshineData( )
  {
    return m_sunshine;
  }

  @Override
  public ITimeseriesCache getWindVelocityData( )
  {
    return m_windVelocity;
  }

  @Override
  public DateRange getDateRange( )
  {
    return m_daterange;
  }

  @Override
  public double getLatitude( )
  {
    return m_latitude;
  }

  /**
   * @see org.kalypso.model.hydrology.operation.evaporation.ICalculateEvaporationData#getStation()
   */
  @Override
  public IStation getStation( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public String getQuality( )
  {
    return null;
  }
}