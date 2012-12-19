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
package org.kalypso.kalypsomodel1d2d.schema.binding.flowrel;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import org.kalypso.commons.math.LinearEquation;
import org.kalypso.commons.math.LinearEquation.SameXValuesException;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.observation.result.TupleResultUtilities;

/**
 * A helper class that wraps the weir-observation and gives access to it in the sense of the weir-parameter philosophie.
 *
 * @author Gernot Belger
 */
public class BuildingParameters
{
  public static final String COMPONENT_DISCHARGE = Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE;

  public static final String COMPONENT_WATERLEVEL_DOWNSTREAM = Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL_DOWNSTREAM;

  public static final String COMPONENT_WATERLEVEL_UPSTREAM = Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL_UPSTREAM;

  private static final BigDecimal UNKNOWN_DISCHARGE = new BigDecimal( "-999.999" ); //$NON-NLS-1$

  private final Map<Object, BigDecimal> m_dischargeMap = new HashMap<>();

  /** Upstream waterlevels, sorted in ascending order. */
  private final SortedSet<BigDecimal> m_upstreamWaterlevels = new TreeSet<>();

  /** Downstream waterlevels, sorted in ascending order. */
  private final SortedSet<BigDecimal> m_downstreamWaterlevels = new TreeSet<>();

  private final IObservation<TupleResult> m_buildingObservation;

  private final Set<BigDecimal> m_dischargeSet = new HashSet<>();

  public BuildingParameters( final IObservation<TupleResult> buildingObservation )
  {
    m_buildingObservation = buildingObservation;

    final TupleResult result = buildingObservation.getResult();

    final int compUpstream = TupleResultUtilities.indexOfComponent( result, COMPONENT_WATERLEVEL_UPSTREAM );
    final int compDownstream = TupleResultUtilities.indexOfComponent( result, COMPONENT_WATERLEVEL_DOWNSTREAM );
    final int compDischarge = TupleResultUtilities.indexOfComponent( result, COMPONENT_DISCHARGE );

    for( final IRecord record : result )
    {
      final BigDecimal upstream = (BigDecimal) record.getValue( compUpstream );
      final BigDecimal downstream = (BigDecimal) record.getValue( compDownstream );
      final BigDecimal discharge = (BigDecimal) record.getValue( compDischarge );

      m_upstreamWaterlevels.add( upstream );
      m_downstreamWaterlevels.add( downstream );

      final Object createWaterlevelTupel = createWaterlevelTupel( upstream, downstream );
      m_dischargeMap.put( createWaterlevelTupel, discharge );

      m_dischargeSet.add( discharge );
    }
  }

  private Object createWaterlevelTupel( final BigDecimal upstream, final BigDecimal downstream )
  {
    final ArrayList<BigDecimal> tuple = new ArrayList<>( 2 );
    tuple.add( upstream );
    tuple.add( downstream );
    return tuple;
  }

  /**
   * Returns all known upstream waterlevels, sorted by size.
   */
  public BigDecimal[] getUpstreamWaterlevels( )
  {
    return m_upstreamWaterlevels.toArray( new BigDecimal[m_upstreamWaterlevels.size()] );
  }

  /**
   * Returns all known downstream waterlevels, sorted by size.
   */
  public BigDecimal[] getDownstreamWaterlevels( )
  {
    return m_downstreamWaterlevels.toArray( new BigDecimal[m_downstreamWaterlevels.size()] );
  }

  public BigDecimal interpolateDischarge( final BigDecimal upstreamWaterlevel, final BigDecimal downstreamWaterlevel )
  {
    /* First, maybe this tuple exists directly */
    final BigDecimal discharge = getDischarge( upstreamWaterlevel, downstreamWaterlevel );
    if( discharge != null )
      return discharge;

    /* maybe only the upstream waterlevel is bad, we interpolate by upstream waterlevel */

    /* First, find a smaller upstream waterlevel which has a discharge with the given downstream waterlevel */
    final SortedSet<BigDecimal> headSet = m_upstreamWaterlevels.headSet( upstreamWaterlevel );
    final BigDecimal[] prevWaterlevels = headSet.toArray( new BigDecimal[headSet.size()] );
    BigDecimal previousDischarge = null;
    BigDecimal previousUpstreamWaterlevel = null;
    for( int i = prevWaterlevels.length; i > 0; i-- )
    {
      previousUpstreamWaterlevel = prevWaterlevels[i - 1];
      previousDischarge = getDischarge( previousUpstreamWaterlevel, downstreamWaterlevel );
      if( previousDischarge != null )
        break;
    }

    if( previousDischarge == null )
      return UNKNOWN_DISCHARGE;

    /* Second, find a bigger upstream waterlevel which has a discharge with the given downstream waterlevel */
    final SortedSet<BigDecimal> tailSet = m_upstreamWaterlevels.tailSet( upstreamWaterlevel );
    BigDecimal nextDischarge = null;
    BigDecimal nextUpstreamWaterlevel = null;

    for( final BigDecimal bigDecimal : tailSet )
    {
      nextUpstreamWaterlevel = bigDecimal;
      nextDischarge = getDischarge( nextUpstreamWaterlevel, downstreamWaterlevel );
      if( nextDischarge != null )
        break;
    }

    if( nextDischarge == null )
      return UNKNOWN_DISCHARGE;

    /* We have a lower and a upper upstream waterlevel with discharge, so interpolate it! */
    try
    {
      final LinearEquation linearEquation = new LinearEquation( previousUpstreamWaterlevel.doubleValue(), previousDischarge.doubleValue(), nextUpstreamWaterlevel.doubleValue(), nextDischarge.doubleValue() );
      return new BigDecimal( linearEquation.computeY( upstreamWaterlevel.doubleValue() ) );
    }
    catch( final SameXValuesException e )
    {
      // this cannot happen, due to definitions of tail and head sets.
      e.printStackTrace();
    }

    // TODO: discuss with nico/moni what to do about unknown values
    // System.out.format( "Unknown discharge for: (US: %.3f DW: %.3f)%n", upstreamWaterlevel, downstreamWaterlevel );

    return UNKNOWN_DISCHARGE;
  }

  private BigDecimal getDischarge( final BigDecimal upstreamWaterlevel, final BigDecimal downstreamWaterlevel )
  {
    final Object tupel = createWaterlevelTupel( upstreamWaterlevel, downstreamWaterlevel );
    final BigDecimal discharge = m_dischargeMap.get( tupel );
    return discharge;
  }

  public TupleResult getValues( )
  {
    return m_buildingObservation.getResult();
  }

  public int getDischargeCount( )
  {
    return m_dischargeSet.size();
  }

}
