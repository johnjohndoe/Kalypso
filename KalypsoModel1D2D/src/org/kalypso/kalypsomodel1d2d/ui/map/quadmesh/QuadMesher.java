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
package org.kalypso.kalypsomodel1d2d.ui.map.quadmesh;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.jts.QuadMesher.JTSQuadMesher;
import org.kalypso.kalypsomodel1d2d.ui.map.grid.LinePointCollector;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;

/**
 * Builds QuadMeshes from various sources.
 *
 * @author Gernot Belger
 * @author Thomas Jung
 */
public class QuadMesher
{
  private QuadMesh m_mesh;

  /**
   * config the with its side points.
   *
   * @param topSidePoints
   *          the collector containing the top side points
   * @param bottomSidePoints
   *          the collector containing the bottom side points
   * @param leftSidePoints
   *          the collector containing the to left side points
   * @param rightSidePoints
   *          the collector containing the to right side points
   * @throws IllegalArgumentException
   *           if one the the side point collector is null
   */
  public IStatus createMesh( final String srsName, final LinePointCollector topSidePoints, final LinePointCollector bottomSidePoints, final LinePointCollector leftSidePoints, final LinePointCollector rightSidePoints )
  {
    final LineString topLine = pointToLineString( topSidePoints );
    final LineString bottomLine = pointToLineString( bottomSidePoints );
    final LineString leftLine = pointToLineString( leftSidePoints );
    final LineString rightLine = pointToLineString( rightSidePoints );

    return createMesh( srsName, leftLine, topLine, rightLine, bottomLine );
  }

  public IStatus createMesh( final String srsName, final LineString leftLine, final LineString topLine, final LineString rightLine, final LineString bottomLine )
  {
    final Coordinate[][] gridPoints = calculateMesh( leftLine, topLine, rightLine, bottomLine );

    m_mesh = new QuadMesh( gridPoints, srsName );

    return Status.OK_STATUS;
  }

  /**
   * get the {@link LinePointCollector} points as {@link LineString}
   */
  private LineString pointToLineString( final LinePointCollector lineGeometryBuilder )
  {
    final int size = lineGeometryBuilder.getCurrentPointCnt();

    final Coordinate coordinates[] = new Coordinate[size];
    for( int i = 0; i < size; i++ )
      coordinates[i] = JTSAdapter.export( lineGeometryBuilder.getPointAt( i ).getPosition() );

    return new com.vividsolutions.jts.geom.GeometryFactory().createLineString( coordinates );
  }

  public QuadMesh getMesh( )
  {
    return m_mesh;
  }

  private Coordinate[][] calculateMesh( final LineString leftLine, final LineString topLine, final LineString rightLine, final LineString bottomLine )
  {
    /* arrange the lines for the mesher */
    /*
     * -the lines have to be oriented (clw or cclw), the order ist top (profile) - left (bank) - bottom (profile) -
     * right (bank) or top - right - bottom - left -the end point of a line must be the same as the start point of the
     * next line -the lines itself must be also oriented all in the same way (clw/cclw)
     */

    // at the end point of the profile line there should follow the start point of the next line
    final LineString[] fixedLines = fixeLineOrientation( topLine, rightLine, bottomLine, leftLine );
    final LineString[] fixedLines2 = fixeLineEndPoints( fixedLines );

    final LineString fixedTopLine = fixedLines2[0];
    final LineString fixedRightLine = fixedLines2[1];
    final LineString fixedBottomLine = fixedLines2[2];
    final LineString fixedLeftLine = fixedLines2[3];

    // compute mesh points
    final JTSQuadMesher mesher = new JTSQuadMesher( fixedTopLine, fixedBottomLine, fixedLeftLine, fixedRightLine );
    return mesher.calculateMesh();
  }

  /** Fix the lines so the endpoints are really the same. */
  private LineString[] fixeLineEndPoints( final LineString[] lines )
  {
    final LineString[] fixedLines = new LineString[lines.length];

    fixedLines[0] = lines[0];

    /* fix start crds of following lines */
    for( int i = 0; i < lines.length - 1; i++ )
    {
      final LineString line = lines[i + 1];
      final Coordinate[] crds = line.getCoordinates();

      crds[0] = lines[i].getCoordinateN( lines[i].getNumPoints() - 1 );

      /* fix end point of last line */
      if( i == lines.length - 2 )
      {
        crds[crds.length - 1] = lines[0].getCoordinateN( 0 );
      }

      fixedLines[i + 1] = line.getFactory().createLineString( crds );
    }


    return fixedLines;
  }

  private LineString[] fixeLineOrientation( final LineString... lines )
  {
    final LineString[] fixedLines = new LineString[lines.length];

    final List<LineString> searchLines = new ArrayList<>( Arrays.asList( lines ) );

    fixedLines[0] = lines[0];

    for( int i = 0; i < fixedLines.length - 1; i++ )
      fixedLines[i + 1] = findAdjacentLine( fixedLines[i], searchLines );

    return fixedLines;
  }

  /**
   * Finds a line in a list of lines that has the same start/end point as the end point of the given line.<br/>
   * The found line is removed from the given list.
   *
   * @return the found line or <code>null</code> if none. The returned line is oriented so, that the start point of that
   *         line is the same as the end point of the given line.
   */
  private LineString findAdjacentLine( final LineString line, final List<LineString> lines )
  {
    final Point endPoint = line.getEndPoint();

    for( final Iterator<LineString> iterator = lines.iterator(); iterator.hasNext(); )
    {
      final LineString searchLine = iterator.next();
      if( searchLine == line )
        continue;

      /* If start point fits, directly reutrn this line */
      final Point searchStart = searchLine.getStartPoint();
      if( searchStart.distance( endPoint ) < 0.01 )
      {
        iterator.remove();
        return searchLine;
      }

      /* If end point fits, return this line, but reverse it */
      final Point searchEnd = searchLine.getEndPoint();
      if( searchEnd.distance( endPoint ) < 0.01 )
      {
        iterator.remove();
        return (LineString) searchLine.reverse();
      }
    }

    return null;
  }
}