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
package org.kalypso.jts;

import org.kalypso.commons.math.LinearEquation;
import org.kalypso.commons.math.LinearEquation.SameXValuesException;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;

/**
 * Utility class for some geometry operations.
 * 
 * @author Holger Albert
 */
public class JTSUtilities
{
  private JTSUtilities( )
  {
  }

  /**
   * This function delivers the first point from a line in another geometry.
   * 
   * @param line
   *          The points of this line will be checked. The first, which lies in the given geometry is returned.
   * @param geometry_2nd
   *          The points of the line will be checked with this geometry.
   * @return The first point of the line, which lies in the second geometry.
   */
  public static com.vividsolutions.jts.geom.Point linePointInGeometry( LineString line, Geometry geometry_2nd )
  {
    int numPoints = line.getNumPoints();

    for( int i = 0; i < numPoints; i++ )
    {
      Point pointN = line.getPointN( i );

      if( geometry_2nd.contains( pointN ) )
        return new Point( new Coordinate( pointN.getCoordinate() ), pointN.getPrecisionModel(), pointN.getSRID() );
    }

    return null;
  }

  /**
   * This function calculates a point at a specific length of a line.
   * 
   * @param lineJTS
   *          The line string on which the point has to be.
   * @param distance
   *          The distance at which the point should be placed on the line.
   * @return The newly created point on the line or null, if something was wrong.
   */
  public static Point pointOnLine( LineString lineJTS, double distance )
  {
    double length = lineJTS.getLength();

    if( distance < 0 || distance > length )
      return null;

    int numPoints = lineJTS.getNumPoints();

    /* Only loop until the point before the last point. */
    LineString line = null;
    for( int i = 0; i < numPoints - 1; i++ )
    {
      Point startPoint = lineJTS.getPointN( i );
      Point endPoint = lineJTS.getPointN( i + 1 );

      line = new LineString( new Coordinate[] { new Coordinate( startPoint.getCoordinate() ), new Coordinate( endPoint.getCoordinate() ) }, lineJTS.getPrecisionModel(), lineJTS.getSRID() );
      double lineLength = line.getLength();

      if( distance - lineLength < 0 )
        break;

      distance -= lineLength;
    }

    /* Now calculate the rest of the line. */
    double max = line.getLength();

    Point startPoint = line.getStartPoint();
    Point endPoint = line.getEndPoint();

    try
    {
      /* If the two X koords are equal, take one of them for the new point. */
      double x = startPoint.getX();
      if( Double.compare( startPoint.getX(), endPoint.getX() ) != 0 )
      {
        LinearEquation computeX = new LinearEquation( startPoint.getX(), 0, endPoint.getX(), max );
        x = computeX.computeX( distance );
      }

      /* If the two Y koords are equal, take one of them for the new point. */
      double y = startPoint.getY();
      if( Double.compare( startPoint.getY(), endPoint.getY() ) != 0 )
      {
        LinearEquation computeY = new LinearEquation( startPoint.getY(), 0, endPoint.getY(), max );
        y = computeY.computeX( distance );
      }

      Point pointJTS = new Point( new Coordinate( x, y ), lineJTS.getPrecisionModel(), lineJTS.getSRID() );

      return pointJTS;
    }
    catch( SameXValuesException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

    return null;
  }

  /**
   * This function calculates a point at a specific length of a line.
   * 
   * @param lineJTS
   *          The line string on which the point has to be.
   * @param percent
   *          The distance in percent at which the point should be placed on the line.
   * @return The newly created point on the line or null, if something was wrong.
   */
  public static com.vividsolutions.jts.geom.Point pointOnLinePercent( LineString lineJTS, int percent )
  {
    if( percent < 0 || percent > 100 )
      return null;

    double length = lineJTS.getLength();
    double distance = length / 100.0 * percent;

    return pointOnLine( lineJTS, (int) distance );
  }
}