/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.ui.rrm.internal.conversion.to12_02;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.joda.time.Interval;
import org.joda.time.LocalTime;
import org.joda.time.Period;
import org.kalypso.model.hydrology.binding.cm.ILinearSumGenerator;

/**
 * Helper that gathers some statistics (timestep, smalles period etc.) for a catchment model.
 * 
 * @author Gernot Belger
 */
class GeneratorStatistics
{
  /* Memory for the timestamps. */
  private final Map<LocalTime, Integer> m_timestamps = new HashMap<>();

  /* Very big period, so everything is smaller. */
  private Period m_smallestTimestep = Period.days( 10000 );

  private Interval m_validityRange = null;

  public void addTimeseriesData( final Period timestep, final LocalTime timestamp, final Interval dateRange )
  {
    if( timestep != null )
    {
      if( timestep.toStandardSeconds().getSeconds() < m_smallestTimestep.toPeriod().toStandardSeconds().getSeconds() )
        m_smallestTimestep = timestep;
    }

    if( timestamp != null )
    {
      final Integer number = m_timestamps.get( timestamp );
      if( number != null )
        m_timestamps.put( timestamp, new Integer( number.intValue() + 1 ) );
      else
        m_timestamps.put( timestamp, new Integer( 1 ) );
    }

    if( dateRange != null )
    {
      if( m_validityRange == null )
        m_validityRange = dateRange;
      else
        m_validityRange = m_validityRange.overlap( dateRange );
    }
  }

  public void apply( final ILinearSumGenerator generator )
  {
    /* Use the smallest period of all involved timeseries as timestep for the generator. */
    final int timestepMinutes = m_smallestTimestep.toPeriod().toStandardMinutes().getMinutes();
    if( timestepMinutes > 0 )
      generator.setTimestep( timestepMinutes );

    /* Set the timestamp. */
    final LocalTime timestamp = findMostUsedTimestamp( m_timestamps );
    if( timestamp != null )
      generator.setTimestamp( timestamp );

    /* use the union the ranges of all timeseries as the validity range of the generator */
    if( m_validityRange != null )
    {
      generator.setValidFrom( m_validityRange.getStart().toDate() );
      generator.setValidTo( m_validityRange.getEnd().toDate() );
    }
  }

  private LocalTime findMostUsedTimestamp( final Map<LocalTime, Integer> timestamps )
  {
    LocalTime timestamp = null;
    int number = 0;

    final Set<Entry<LocalTime, Integer>> entrySet = timestamps.entrySet();
    for( final Entry<LocalTime, Integer> entry : entrySet )
    {
      final LocalTime key = entry.getKey();
      final Integer value = entry.getValue();

      if( value.intValue() > number )
      {
        timestamp = key;
        number = value.intValue();
      }
    }

    return timestamp;
  }
}