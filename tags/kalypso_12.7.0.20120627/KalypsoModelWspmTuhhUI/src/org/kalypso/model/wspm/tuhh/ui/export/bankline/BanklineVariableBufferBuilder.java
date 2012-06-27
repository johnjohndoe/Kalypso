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
package org.kalypso.model.wspm.tuhh.ui.export.bankline;

import org.eclipse.core.runtime.Assert;
import org.kalypso.commons.math.geom.PolyLine;

import com.vividsolutions.jts.geom.CoordinateList;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Location;
import com.vividsolutions.jts.operation.buffer.BufferParameters;

/**
 * @author Gernot Belger
 */
public class BanklineVariableBufferBuilder
{
  private final GeometryFactory m_factory = new GeometryFactory();

  private final LineString m_riverLine;

  private final PolyLine m_leftDistances;

  private final PolyLine m_rightDistances;

  private final BufferParameters m_bufferParams = new BufferParameters( BufferParameters.DEFAULT_QUADRANT_SEGMENTS, BufferParameters.CAP_ROUND, BufferParameters.JOIN_ROUND, BufferParameters.DEFAULT_MITRE_LIMIT );

  public BanklineVariableBufferBuilder( final LineString riverLine, final PolyLine leftDistances, final PolyLine rightDistances )
  {
    Assert.isNotNull( leftDistances );
    Assert.isNotNull( rightDistances );

    m_riverLine = riverLine;
    m_leftDistances = leftDistances;
    m_rightDistances = rightDistances;
  }

  public Geometry buffer( )
  {
    final double maxLeftDistance = getMaxDistance( m_leftDistances );
    final double maxRightDistance = getMaxDistance( m_rightDistances );
    final double maxDistance = Math.max( maxLeftDistance, maxRightDistance );

    final LineString simpleLine = m_riverLine;

    final LineString denseRiverLine = simpleLine;

    final VariableOffsetCurveBuilder leftCurveBuilder = new VariableOffsetCurveBuilder( m_leftDistances, -1, m_bufferParams );
    leftCurveBuilder.addSegments( denseRiverLine.getCoordinates() );

    final VariableOffsetCurveBuilder rightCurveBuilder = new VariableOffsetCurveBuilder( m_rightDistances, +1, m_bufferParams );
    rightCurveBuilder.addSegments( denseRiverLine.getCoordinates() );

    /* Build closed ring */
    final CoordinateList allCoordinates = new CoordinateList();
    allCoordinates.add( leftCurveBuilder.getVertices(), false, true );
    allCoordinates.add( rightCurveBuilder.getVertices(), false, false );
    /* close ring */
    allCoordinates.add( allCoordinates.get( 0 ), true );

    final VariableBufferGeometryBuilder geometryBuilder = new VariableBufferGeometryBuilder( denseRiverLine, maxDistance, m_factory );
    geometryBuilder.addCurve( allCoordinates.toCoordinateArray(), Location.INTERIOR, Location.EXTERIOR );
    return geometryBuilder.computeGeometry();
  }

  private double getMaxDistance( final PolyLine distances )
  {
    double max = Double.MIN_VALUE;

    final double[] y = distances.getY();

    for( final double i : y )
      max = Math.max( max, i );

    return max;
  }
}