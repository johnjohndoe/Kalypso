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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import java.util.ArrayList;
import java.util.List;

import com.infomatiq.jsi.Point;
import com.infomatiq.jsi.Rectangle;
import com.infomatiq.jsi.SpatialIndex;
import com.infomatiq.jsi.rtree.RTree;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * An {@link com.infomatiq.jsi.rtree.RTree} based spatial index for coordinates.
 * 
 * @author Gernot Belger
 */
class CoordinateIndex
{
  private final List<Coordinate> m_coordinates = new ArrayList<>();

  private final SpatialIndex m_index = new RTree();

  public CoordinateIndex( )
  {
    m_index.init( null );
  }

  public void add( final Coordinate point )
  {
    m_coordinates.add( point );

    final float x = (float)point.x;
    final float y = (float)point.y;

    final Rectangle bounds = new Rectangle( x, y, x, y );

    m_index.add( bounds, m_coordinates.size() );
  }

  public int size( )
  {
    return m_index.size();
  }

  public Coordinate findNearestPoint( final Coordinate searchCrd, final float furthestDistance )
  {
    final Point searchPoint = new Point( (float)searchCrd.x, (float)searchCrd.y );

    final NearestCoordinateVisitor visitor = new NearestCoordinateVisitor( m_coordinates, searchCrd );

    m_index.nearest( searchPoint, visitor, furthestDistance );

    return visitor.getResult();
  }
}