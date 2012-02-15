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

import java.util.Set;
import java.util.TreeSet;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.index.SpatialIndex;
import com.vividsolutions.jts.index.quadtree.Quadtree;

/**
 * @author Gernot Belger
 */
public class StatisticCollector
{
  private final Set<Integer> m_returnPeriods = new TreeSet<>();

  private SpatialIndex m_elements;

  private RiskStatisticItem[] m_items;

  private final RiskStatisticItem m_total = new RiskStatisticItem( new StatisticItemKey( "Total", StringUtils.EMPTY ) );

  public void addSpecificDamage( final int returnPeriod, final Coordinate position, final double cellArea )
  {
    m_returnPeriods.add( returnPeriod );

    final SpecificDamageVisitor visitor = new SpecificDamageVisitor( returnPeriod, position, cellArea );

    final Envelope searchEnv = new Envelope( position );
    m_elements.query( searchEnv, visitor );

    m_total.addSpecificDamage( returnPeriod, position.z, cellArea );
  }

  public void addAverageDamage( final Coordinate position, final double cellArea )
  {
    // coordinateSystem = polygon.getCrs();
    // m_geoTransformer = GeoTransformerFactory.getGeoTransformer( coordinateSystem );
    // final GM_Position positionAt = JTSAdapter.wrap( coordinate );
    // final GM_Position position = m_geoTransformer.transform( positionAt, m_resultGrid.getSourceCRS() );

    final AverageDamageVisitor visitor = new AverageDamageVisitor( position, cellArea );

    final Envelope searchEnv = new Envelope( position );
    m_elements.query( searchEnv, visitor );

    m_total.addAverageAnnualDamage( position.z, cellArea );
  }

  public void setItems( final RiskStatisticItem[] items )
  {
    m_items = items;

    final Quadtree quadtree = new Quadtree();

    for( final RiskStatisticItem item : items )
    {
      final StatisticArea[] areas = item.getAreas();
      for( final StatisticArea area : areas )
      {
        final Envelope envelope = area.getArea().getEnvelopeInternal();
        quadtree.insert( envelope, area );
      }
    }

    m_elements = quadtree;
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

}