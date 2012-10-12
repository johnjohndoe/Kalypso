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

import gnu.trove.TIntProcedure;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author Gernot Belger
 */
public class SpecificDamageVisitor implements TIntProcedure
{
  private final int m_returnPeriod;

  private final Coordinate m_position;

  private final double m_cellArea;

  private final StatisticArea[] m_areas;

  public SpecificDamageVisitor( final int returnPeriod, final Coordinate position, final double cellArea, final StatisticArea[] areas )
  {
    m_returnPeriod = returnPeriod;
    m_position = position;
    m_cellArea = cellArea;
    m_areas = areas;
  }

  @Override
  public boolean execute( final int id )
  {
    final StatisticArea areaItem = m_areas[id];
    if( areaItem.contains( m_position ) )
      areaItem.getItem().addSpecificDamage( m_returnPeriod, m_position.z, m_cellArea );

    return true;
  }
}