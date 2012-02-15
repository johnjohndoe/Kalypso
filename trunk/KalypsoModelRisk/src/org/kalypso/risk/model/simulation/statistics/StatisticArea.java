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

import com.vividsolutions.jts.algorithm.PointInRing;
import com.vividsolutions.jts.algorithm.SimplePointInRing;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Represents one polygon area of an item that contains possibly many ones.
 *
 * @author Gernot Belger
 */
public class StatisticArea
{
  private final PointInRing m_pointInRing;

  private final RiskStatisticItem m_statisticItem;

  private final Polygon m_area;

  public StatisticArea( final RiskStatisticItem statisticItem, final Polygon area )
  {
    m_statisticItem = statisticItem;
    m_area = area;
    m_pointInRing = new SimplePointInRing( (LinearRing) area.getExteriorRing() );
  }

  public boolean contains( final Coordinate position )
  {
    final boolean inside = m_pointInRing.isInside( position );
    if( !inside )
      return false;

    if( m_area.getNumInteriorRing() == 0 )
      return true;

    // TODO: slow
    final Point point = m_area.getFactory().createPoint( position );
    return m_area.contains( point );
  }

  public RiskStatisticItem getItem( )
  {
    return m_statisticItem;
  }

  public Polygon getArea( )
  {
    return m_area;
  }
}