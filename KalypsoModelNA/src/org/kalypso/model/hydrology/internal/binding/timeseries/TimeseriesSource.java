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
package org.kalypso.model.hydrology.internal.binding.timeseries;

import org.kalypso.commons.java.lang.Objects;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.util.pool.IPoolableObjectType;
import org.kalypso.core.util.pool.KeyInfo;
import org.kalypso.core.util.pool.ResourcePool;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.timeseries.Timeserieses;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.metadata.ParameterTypeLabelProvider;
import org.kalypso.ogc.sensor.provider.IObsProvider;
import org.kalypso.ogc.sensor.provider.PooledObsProvider;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.zml.core.base.IZmlSourceElement;

/**
 * @author Dirk Kuch
 */
public class TimeseriesSource implements IZmlSourceElement
{
  private final Timeseries m_timeseries;

  private String m_identifier;

  private PooledObsProvider m_provider;

  public TimeseriesSource( final Timeseries timeseries )
  {
    m_timeseries = timeseries;
    m_identifier = timeseries.getParameterType();
  }

  @Override
  public void dispose( )
  {
    // nothing to do...
  }

  @Override
  public IObsProvider getObsProvider( )
  {
    if( Objects.isNotNull( m_provider ) )
      return m_provider;

    final ZmlLink link = m_timeseries.getDataLink();
    final IPoolableObjectType key = link.getPoolableObjectType();
    m_provider = new PooledObsProvider( key );

    return m_provider;
  }

  @Override
  public IPoolableObjectType getPoolKey( )
  {
    final ZmlLink link = m_timeseries.getDataLink();

    return link.getPoolableObjectType();
  }

  @Override
  public boolean isDirty( )
  {
    final ZmlLink link = m_timeseries.getDataLink();
    final IObservation observation = link.getObservationFromPool();
    final ResourcePool pool = KalypsoCorePlugin.getDefault().getPool();
    final KeyInfo info = pool.getInfo( observation );

    return info.isDirty();
  }

  @Override
  public String getLabel( )
  {
    final IStation station = m_timeseries.getStation();
    final String stationName = station.getDescription();
    final String parameterType = m_timeseries.getParameterType();
    final ParameterTypeLabelProvider provider = new ParameterTypeLabelProvider();
    final String parameterLabel = provider.getText( parameterType );

    final String timeseriesLabel = Timeserieses.getTreeLabel( m_timeseries );

    return String.format( "%s\r\n%s - %s", stationName, timeseriesLabel, parameterLabel ); //$NON-NLS-1$
  }

  @Override
  public String getIdentifier( )
  {
    return m_identifier;
  }

  @Override
  public void setIdentifier( final String identifier )
  {
    m_identifier = identifier;
  }

  @Override
  public int getIndex( )
  {
    return 0;
  }

}
