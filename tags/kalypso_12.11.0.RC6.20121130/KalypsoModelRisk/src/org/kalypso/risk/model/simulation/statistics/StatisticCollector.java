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
package org.kalypso.risk.model.simulation.statistics;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Set;
import java.util.TreeSet;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;

import com.infomatiq.jsi.Rectangle;
import com.infomatiq.jsi.SpatialIndex;
import com.infomatiq.jsi.rtree.RTree;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

/**
 * @author Gernot Belger
 */
public class StatisticCollector
{
  private final Set<Integer> m_returnPeriods = new TreeSet<>();

  private RiskStatisticItem[] m_items;

  private final RiskStatisticItem m_total = new RiskStatisticItem( new StatisticItemKey( "Total", StringUtils.EMPTY ) ); //$NON-NLS-1$

  private final String m_srsName;

  private StatisticArea[] m_areas;

  private SpatialIndex m_index;

  /**
   * @param srsName
   *          See {@link #getSRSName()}.
   */
  public StatisticCollector( final String srsName )
  {
    m_srsName = srsName;
  }

  /**
   * @pram position Must be in the coordinate system of {@link #getSRSName()}
   */
  public void addSpecificDamage( final int returnPeriod, final Coordinate position, final double cellArea )
  {
    m_returnPeriods.add( returnPeriod );

    final SpecificDamageVisitor visitor = new SpecificDamageVisitor( returnPeriod, position, cellArea, m_areas );

    final Rectangle searchRect = new Rectangle( (float)position.x, (float)position.y, (float)position.x, (float)position.y );
    m_index.intersects( searchRect, visitor );

    final double damageValue = position.z;

    m_total.addSpecificDamage( returnPeriod, damageValue, cellArea );
  }

  public void setItems( final RiskStatisticItem[] items )
  {
    m_items = items;

    final SpatialIndex index = new RTree();
    index.init( null );

    final Collection<StatisticArea> areaList = new ArrayList<>();

    for( final RiskStatisticItem item : items )
    {
      final StatisticArea[] areas = item.getAreas();
      for( final StatisticArea area : areas )
      {
        final int id = areaList.size();

        final Envelope envelope = area.getArea().getEnvelopeInternal();

        final Rectangle rect = new Rectangle( (float)envelope.getMinX(), (float)envelope.getMinY(), (float)envelope.getMaxX(), (float)envelope.getMaxY() );

        areaList.add( area );
        index.add( rect, id );
      }
    }

    m_areas = areaList.toArray( new StatisticArea[areaList.size()] );
    m_index = index;
  }

  public void createResultObservation( final IRasterizationControlModel controlModel )
  {
    final Integer[] returnPeriods = m_returnPeriods.toArray( new Integer[m_returnPeriods.size()] );
    final StatisticObservationBuilder observationBuilder = new StatisticObservationBuilder( controlModel, returnPeriods, m_items, m_total );
    observationBuilder.execute();
  }

  public RiskStatisticItem[] getItems( )
  {
    return m_items;
  }

  /** The name of the coordinate system all statistic item are in; especially, whenn {@link #addSpecificDamage(int, Coordinate, double)}, th coordinate must be in this system. */
  public String getSRSName( )
  {
    return m_srsName;
  }
}