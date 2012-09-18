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
package org.kalypso.model.wspm.tuhh.core.profile.importer.wprof;

import java.util.Collection;
import java.util.Iterator;
import java.util.SortedMap;
import java.util.TreeMap;

import org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint;

import de.openali.odysseus.chart.framework.model.mapper.registry.impl.NumberComparator;

/**
 * @author Gernot Belger
 */
class ProfilePolygon
{
  private final SortedMap<Number, IWProfPoint> m_points = new TreeMap<>( new NumberComparator() );

  private final String m_objectType;

  public ProfilePolygon( final String objectType )
  {
    m_objectType = objectType;
  }

  public ProfilePolygon( final String objectType, final IWProfPoint[] points )
  {
    m_objectType = objectType;

    for( final IWProfPoint point : points )
    {
      add( point );
    }
  }

  public void add( final IWProfPoint wprofPoint )
  {
    final Number ord = wprofPoint.getNumber();
    m_points.put( ord, wprofPoint );
  }

  public IWProfPoint getFirstPoint( )
  {
    final Iterator<IWProfPoint> iterator = m_points.values().iterator();
    if( iterator.hasNext() )
      return iterator.next();

    return null;
  }

  public IWProfPoint[] getPoints( )
  {
    final Collection<IWProfPoint> values = m_points.values();
    return values.toArray( new IWProfPoint[values.size()] );
  }

// public BridgePoint[] getAsBridgePoints( )
// {
// final Collection<IWProfPoint> points = m_points.values();
// return points.toArray( new IWProfPoint[points.size()] );
// }

// public static BridgePoint[] toBridgePoints( final Collection<IWProfPoint> points )
// {
// final Collection<BridgePoint> bps = new ArrayList<BridgePoint>();
//
// for( final IWProfPoint point : points )
// {
// final BigDecimal distance = point.getDistance();
// final double value = point.getValue();
// final String comment = point.getComment();
// bps.add( new BridgePoint( distance, value, comment ) );
// }
//
// return bps.toArray( new BridgePoint[bps.size()] );
// }

  public String getObjectType( )
  {
    return m_objectType;
  }

  public int size( )
  {
    return m_points.size();
  }

}
