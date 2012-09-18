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
package org.kalypso.model.wspm.tuhh.schema.simulation;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.schema.gml.QIntervallResult;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Gernot Belger
 */
public class QIntervalIndex
{
  private final Map<BigDecimal, QIntervallResult> m_index = new HashMap<>();

  private final SortedMap<BigDecimal, IProfileFeature> m_profileIndex;

  private final IFeatureBindingCollection<QIntervallResult> m_qIntervalls;

  public QIntervalIndex( final IFeatureBindingCollection<QIntervallResult> qIntervalls, final TuhhReach reach )
  {
    m_qIntervalls = qIntervalls;
    m_profileIndex = buildStationIndex( reach );
  }

  public static <T> T forStation( final SortedMap<BigDecimal, T> stationIndex, final BigDecimal station )
  {
    final BigDecimal pred = NumberUtils.decrement( station );
    final BigDecimal succ = NumberUtils.increment( pred );
    final SortedMap<BigDecimal, T> subMap = stationIndex.subMap( pred, succ );
    if( !subMap.isEmpty() )
      return subMap.values().iterator().next();

    return stationIndex.get( station );
  }

  private static SortedMap<BigDecimal, IProfileFeature> buildStationIndex( final TuhhReach reach )
  {
    final TuhhReachProfileSegment[] reachProfileSegments = reach.getReachProfileSegments();
    final SortedMap<BigDecimal, IProfileFeature> index = new TreeMap<>();
    for( final TuhhReachProfileSegment segment : reachProfileSegments )
    {
      final IProfileFeature profileMember = segment.getProfileMember();
      index.put( segment.getStation(), profileMember );
    }

    return index;
  }

  public QIntervallResult addOrGet( final BigDecimal station )
  {
    if( !m_index.containsKey( station ) )
    {
      /* Create a new Point Result if not yet existent */
      final QIntervallResult newQresult = m_qIntervalls.addNew( QIntervallResult.QNAME_F_QIntervallResult );
      newQresult.setStation( station );
      newQresult.setName( station.toString() );
      m_index.put( station, newQresult );

      /* Link to profile */
      final IProfileFeature profile = forStation( m_profileIndex, station );
      if( profile != null )
        newQresult.setProfileLink( profile );
    }

    return m_index.get( station );
  }

  public QIntervallResult get( final BigDecimal station )
  {
    return m_index.get( station );
  }
}