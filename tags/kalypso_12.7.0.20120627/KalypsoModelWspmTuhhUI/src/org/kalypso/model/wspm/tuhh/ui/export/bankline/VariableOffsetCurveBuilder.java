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
package org.kalypso.model.wspm.tuhh.ui.export.bankline;

import org.kalypso.commons.math.geom.PolyLine;
import org.kalypso.jts.JtsVectorUtilities;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateList;
import com.vividsolutions.jts.geom.LineSegment;
import com.vividsolutions.jts.operation.buffer.BufferParameters;

/**
 * @author Gernot Belger
 */
public class VariableOffsetCurveBuilder
{
  private final CoordinateList m_bufferVertices = new CoordinateList();

  private final double m_distanceSignum;

  private final BufferParameters m_bufferParams;

  private final PolyLine m_distances;

  public VariableOffsetCurveBuilder( final PolyLine distances, final double distanceSignum, final BufferParameters bufferParams )
  {
    m_distances = distances;
    m_distanceSignum = distanceSignum;
    m_bufferParams = bufferParams;
  }

  public void addSegments( final Coordinate[] coordinates )
  {
    /* Add first vertex */
    if( coordinates.length > 0 )
    {
      final LineSegment firstSegment = new LineSegment( coordinates[0], coordinates[1] );
      final Coordinate firstVertex = JtsVectorUtilities.getOrthogonalPoint( firstSegment, firstSegment.p0, getDistance( 0.0 ) );
      addVertices( firstVertex );
    }

    final LineSegmentPair cursor = new LineSegmentPair( this, 0.01, m_bufferParams );

    for( int i = 0; i < coordinates.length - 1; i++ )
    {
      final LineSegment segment = new LineSegment( coordinates[i], coordinates[i + 1] );
      cursor.pushSegment( segment );
    }

    /* Add last vertex */
    if( coordinates.length > 1 )
    {
      final LineSegment lastSegment = new LineSegment( coordinates[coordinates.length - 2], coordinates[coordinates.length - 1] );

      final double lastDistance = cursor.getDistance2();

      final Coordinate lastVertex = JtsVectorUtilities.getOrthogonalPoint( lastSegment, lastSegment.p1, lastDistance );
      addVertices( lastVertex );
    }
  }

  void addVertices( final Coordinate... vertices )
  {
    for( final Coordinate coordinate : vertices )
    {
      if( Double.isNaN( coordinate.x ) || Double.isNaN( coordinate.y ) )
        return;
    }

    m_bufferVertices.add( vertices, false );
  }

  double getDistance( final double station )
  {
    return m_distances.getYFor( station ) * m_distanceSignum;
  }

  double getDistanceSignum( )
  {
    return m_distanceSignum;
  }

  public Coordinate[] getVertices( )
  {
    return m_bufferVertices.toCoordinateArray();
  }
}