/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Bj�rnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universit�t Hamburg-Harburg, Institut f�r Wasserbau, Hamburg, Germany
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

import gnu.trove.TIntProcedure;

import java.util.List;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * Visitor for finding the nearest coordinate.
 * 
 * @author Gernot Belger
 */
class NearestCoordinateVisitor implements TIntProcedure
{
  private final List<Coordinate> m_coordinates;

  private final Coordinate m_searchCoordinate;

  private Coordinate m_result = null;

  private double m_resultDistance = Double.MAX_VALUE;

  public NearestCoordinateVisitor( final List<Coordinate> coordinates, final Coordinate searchCoordinate )
  {
    m_coordinates = coordinates;
    m_searchCoordinate = searchCoordinate;
  }

  @Override
  public boolean execute( final int value )
  {
    final Coordinate foundCoordinate = m_coordinates.get( value - 1 );
    final double foundDistance = m_searchCoordinate.distance( foundCoordinate );

    if( foundDistance < m_resultDistance )
    {
      m_result = foundCoordinate;
      m_resultDistance = foundDistance;
    }

    return true;
  }

  public Coordinate getResult( )
  {
    return m_result;
  }
}