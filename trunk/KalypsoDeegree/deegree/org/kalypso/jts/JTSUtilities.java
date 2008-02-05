/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.runtime.Assert;
import org.kalypso.commons.math.LinearEquation;
import org.kalypso.commons.math.LinearEquation.SameXValuesException;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineSegment;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.MultiLineString;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;

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
   *            The points of this line will be checked. The first, which lies in the given geometry is returned.
   * @param geometry_2nd
   *            The points of the line will be checked with this geometry.
   * @return The first point of the line, which lies in the second geometry.
   */
  public static Point linePointInGeometry( final LineString line, final Geometry geometry_2nd )
  {
    final int numPoints = line.getNumPoints();

    for( int i = 0; i < numPoints; i++ )
    {
      final Point pointN = line.getPointN( i );

      if( geometry_2nd.contains( pointN ) )
      {
        final GeometryFactory factory = new GeometryFactory( pointN.getPrecisionModel(), pointN.getSRID() );
        return factory.createPoint( new Coordinate( pointN.getCoordinate() ) );
      }
    }

    return null;
  }

  /**
   * @param lineString
   * @param point
   *            of lineString
   * @param buffer
   *            point buffer - point almost doesn't intersects lineString
   * @return distance of point on lineString
   */
  public static Double pointDistanceOnLine( final LineString lineString, final Point point, final double buffer )
  {
    final Polygon geo = (Polygon) point.buffer( buffer );
    if( !lineString.intersects( geo ) )
      throw new IllegalStateException();

    final GeometryFactory GF = new GeometryFactory();

    final Coordinate[] coordinates = lineString.getCoordinates();
    for( int i = 1; i < coordinates.length; i++ )
    {
      final Coordinate[] c = (Coordinate[]) ArrayUtils.subarray( coordinates, 0, i + 1 );

      final LineString ls = GF.createLineString( c );
      if( !ls.intersects( geo ) )
        continue;

      final List<Coordinate> list = new ArrayList<Coordinate>();
      for( int j = 0; j < c.length - 1; j++ )
      {
        list.add( c[j] );
      }

      list.add( point.getCoordinate() );

      final LineString myLineString = GF.createLineString( list.toArray( new Coordinate[] {} ) );

      return myLineString.getLength();
    }

    throw new IllegalStateException();
  }

  /**
   * This function calculates a point at a specific length of a line.
   * 
   * @param lineJTS
   *            The line string on which the point has to be.
   * @param distance
   *            The distance at which the point should be placed on the line.
   * @return The newly created point on the line or null, if something was wrong.
   */
  public static Point pointOnLine( final LineString lineJTS, double distance )
  {
    final double length = lineJTS.getLength();

    if( distance < 0 || distance > length )
      return null;

    final int numPoints = lineJTS.getNumPoints();

    if( numPoints == 0 )
      return null;

    /* Only loop until the point before the last point. */
    LineString line = null;
    for( int i = 0; i < numPoints - 1; i++ )
    {
      final Point startPoint = lineJTS.getPointN( i );
      final Point endPoint = lineJTS.getPointN( i + 1 );

      final GeometryFactory factory = new GeometryFactory( lineJTS.getPrecisionModel(), lineJTS.getSRID() );
      line = factory.createLineString( new Coordinate[] { new Coordinate( startPoint.getCoordinate() ), new Coordinate( endPoint.getCoordinate() ) } );
      final double lineLength = line.getLength();

      if( distance - lineLength < 0 )
        break;

      distance -= lineLength;
    }

    /* Now calculate the rest of the line. */
    final double max = line.getLength();

    final Point startPoint = line.getStartPoint();
    final Point endPoint = line.getEndPoint();

    try
    {
      /* If the two X koords are equal, take one of them for the new point. */
      double x = startPoint.getX();
      if( Double.compare( startPoint.getX(), endPoint.getX() ) != 0 )
      {
        final LinearEquation computeX = new LinearEquation( startPoint.getX(), 0, endPoint.getX(), max );
        x = computeX.computeX( distance );
      }

      /* If the two Y koords are equal, take one of them for the new point. */
      double y = startPoint.getY();
      if( Double.compare( startPoint.getY(), endPoint.getY() ) != 0 )
      {
        final LinearEquation computeY = new LinearEquation( startPoint.getY(), 0, endPoint.getY(), max );
        y = computeY.computeX( distance );
      }
      /* If the two Z koords are equal, take one of them for the new point. */
      double zStart = startPoint.getCoordinate().z;
      final double zEnd = endPoint.getCoordinate().z;

      if( zStart != Double.NaN && zEnd != Double.NaN )
      {
        if( Double.compare( zStart, zEnd ) != 0 )
        {
          final LinearEquation computeZ = new LinearEquation( zStart, 0, zEnd, max );
          zStart = computeZ.computeX( distance );
        }
      }
      else
        zStart = Double.NaN;

      final GeometryFactory factory = new GeometryFactory( lineJTS.getPrecisionModel(), lineJTS.getSRID() );
      final Point pointJTS = factory.createPoint( new Coordinate( x, y, zStart ) );

      return pointJTS;
    }
    catch( final SameXValuesException e )
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
   *            The line string on which the point has to be.
   * @param percent
   *            The distance in percent at which the point should be placed on the line.
   * @return The newly created point on the line or null, if something was wrong. Update: returns the start point or the
   *         end point if percentage is 0 or 100.
   */
  public static Point pointOnLinePercent( final LineString lineJTS, final int percent )
  {
    if( percent < 0 || percent > 100 )
      return null;

    final double length = lineJTS.getLength();
    final double distance = length / 100.0 * percent;

    if( percent == 0 )
      return lineJTS.getPointN( 0 );
    if( percent == 100 )
      return lineJTS.getPointN( lineJTS.getNumPoints() - 1 );

    return pointOnLine( lineJTS, distance );
  }

  /**
   * This function creates a line segment (JTS) of a line from a given start point to an end point, including all points
   * on the given line.
   * 
   * @param line
   *            The original line.
   * @param start
   *            The start point of the new line (it has to be one point that lies on the original line).
   * @param end
   *            The end point of the new line (it has to be one point that lies on the original line). TODO: the used
   *            distance is calculated only by the x- and y-coordinates!! for an 3-dimensaional distance calculation,
   *            the start and end point should have z-coordinates.
   */
  public static LineString createLineSegment( final Geometry line, final Point start, final Point end )
  {
    /* Check if both points are lying on the line (2d!). */
    if( (line.distance( start ) >= 10E-08) || (line.distance( start ) >= 10E-08) )
      return null;

    if( line instanceof LineString )
    {
      /* Check the orientation of the line. */
      if( !getLineOrientation( (LineString) line, start, end ) )
        return createLineSegmentFromLine( (LineString) line, end, start );

      return createLineSegmentFromLine( (LineString) line, start, end );
    }
    else if( line instanceof MultiLineString )
      return createLineSegmentFromMultiLine( (MultiLineString) line, start, end );

    return null;
  }

  /**
   * Evaluates the two given points and returns true, if the direction is equal of that from line (its points).
   * 
   * @param line
   *            The original LineString.
   * @param start
   *            The start point of the new line (it has to be one point that lies on the original LineString).
   * @param end
   *            The end point of the new line (it has to be one point that lies on the original LineString).
   * @return True, if the first found point of the line is nearer to the start point, than to the end point.
   */
  public static boolean getLineOrientation( final LineString line, final Point start, final Point end )
  {
    /* Check if both points are lying on the line. */
    if( (line.distance( start ) >= 10E-08) || (line.distance( start ) >= 10E-08) )
      throw new IllegalArgumentException( "One of the two points does not lie on the given line ..." );

    boolean first = false;

    for( int i = 0; i < line.getNumPoints() - 1; i++ )
    {
      final Point pointN = line.getPointN( i );
      final Point pointN1 = line.getPointN( i + 1 );

      /* Build a line with the two points to check the flag. */
      final LineSegment testLine = new LineSegment( new Coordinate( pointN.getCoordinate() ), new Coordinate( pointN1.getCoordinate() ) );

      if( testLine.distance( start.getCoordinate() ) < 10E-08 )
        first = true;

      if( testLine.distance( end.getCoordinate() ) < 10E-08 )
      {
        /* The direction is inverse. */
        if( !first )
          return false;

        break;
      }
    }

    return true;
  }

  /**
   * This function creates a LineString (JTS) of a LineString from a given start point to an end point, including all
   * points on the given LineString. However it does not check the orientation of the start and end point. This must be
   * done before calling this method. Use {@link JTSUtilities#getLineOrientation(LineString, Point, Point)} for this
   * operation. Both points should have the same orientation than the line, otherwise the new line has only two points,
   * namly the start and end point.
   * 
   * @param line
   *            The original LineString.
   * @param start
   *            The start point of the new line (it has to be one point that lies on the original LineString).
   * @param end
   *            The end point of the new line (it has to be one point that lies on the original LineString).
   * @return A LineString on the original LineString starting at with the start point and ending with the end point.
   */
  private static LineString createLineSegmentFromLine( final LineString line, final Point start, final Point end )
  {
    final List<Point> points = new LinkedList<Point>();

    boolean add = false;

    points.add( start );

    for( int i = 0; i < line.getNumPoints() - 1; i++ )
    {
      final Point pointN = line.getPointN( i );
      final Point pointN1 = line.getPointN( i + 1 );

      if( add )
        points.add( pointN );

      /* Build a line with the two points to check the flag. */
      final LineSegment testLine = new LineSegment( new Coordinate( pointN.getCoordinate() ), new Coordinate( pointN1.getCoordinate() ) );

      if( testLine.distance( start.getCoordinate() ) < 10E-08 )
        add = true;

      if( testLine.distance( end.getCoordinate() ) < 10E-08 )
      {
        add = false;
        break;
      }
    }

    points.add( end );

    /* Create the coordinates for the new line string. */
    final Coordinate[] coordinates = new Coordinate[points.size()];

    for( int i = 0; i < points.size(); i++ )
      coordinates[i] = new Coordinate( points.get( i ).getCoordinate() );

    final GeometryFactory factory = new GeometryFactory( line.getPrecisionModel(), line.getSRID() );

    return factory.createLineString( coordinates );
  }

  /**
   * This function creates a LineString (JTS) of a MultiLineString from a given start point to an end point, including
   * all points on the given MultiLineString. Gaps between the different LineStrings will be connected in the result, if
   * it should contain more than one LineString of the original MultiLineString. However it does not check the
   * orientation of the start and end point.<br>
   * <br>
   * KNOWN ISSUE:<br>
   * It is possible that this function produce errors, if the start or the end point lies in a gap between two
   * LineStrings. The two points of the gap will not be checked for that point. The result than, will possibly be
   * nonsense. <br>
   * <br>
   * ATTENTION:<br>
   * This class is strange, because creating a LineString part of a MultiLineString should be normally done by
   * dissolving the MultiLineString in one LineString-Object and getting the LineString part of it.<br>
   * There can not be quaranteed, that this function works error free!
   * 
   * @param line
   *            The original MultiLineString.
   * @param start
   *            The start point of the new line (it has to be one point on the original MultiLineString).
   * @param end
   *            The end point of the new line (it has to be one point on the original MultiLineString).
   * @return A LineString on the original MultiLineString starting at with the start point and ending with the end
   *         point.
   */
  private static LineString createLineSegmentFromMultiLine( final MultiLineString line, final Point start, final Point end )
  {
    final List<Point> points = new LinkedList<Point>();

    boolean add = false;
    boolean endPointFound = false;

    points.add( start );
    for( int i = 0; i < line.getNumGeometries(); i++ )
    {
      final LineString lineN = (LineString) line.getGeometryN( i );

      for( int j = 0; j < lineN.getNumPoints() - 1; j++ )
      {
        final Point pointN = lineN.getPointN( j );
        final Point pointN1 = lineN.getPointN( j + 1 );

        if( add )
          points.add( pointN );

        /* Build a line with the two points to check the flag. */
        final LineSegment testLine = new LineSegment( new Coordinate( pointN.getCoordinate() ), new Coordinate( pointN1.getCoordinate() ) );

        if( testLine.distance( start.getCoordinate() ) < 10E-08 )
          add = true;

        if( testLine.distance( end.getCoordinate() ) < 10E-08 )
        {
          add = false;
          endPointFound = true;
          break;
        }
      }

      if( endPointFound )
        break;
    }

    points.add( end );

    /* Create the coordinates for the new line string. */
    final Coordinate[] coordinates = new Coordinate[points.size()];

    for( int i = 0; i < points.size(); i++ )
      coordinates[i] = new Coordinate( points.get( i ).getCoordinate() );
    final GeometryFactory factory = new GeometryFactory( line.getPrecisionModel(), line.getSRID() );

    return factory.createLineString( coordinates );
  }

  /**
   * Returns a vector of this line.
   * 
   * @param start
   *            The start point of the line.
   * @param end
   *            The end point of the line.
   * @return A vector of the line between this two points as point.
   */
  public static Point getVector( final Point start, final Point end )
  {
    final Coordinate coords = new Coordinate( start.getX() - end.getX(), start.getY() - end.getY() );
    final GeometryFactory factory = new GeometryFactory( start.getPrecisionModel(), start.getSRID() );

    return factory.createPoint( coords );
  }

  /**
   * Calculates a normalized vector.
   * 
   * @param vector
   *            The vector to be normalized.
   * @return The normalized vector.
   */
  public static Point getNormalizedVector( final Point vector )
  {
    final double x = vector.getX();
    final double y = vector.getY();

    /* The length of a vector is the sum of all elements with the power of two and than the square root of it. */
    final double laenge = Math.sqrt( x * x + y * y );

    final Coordinate coord = new Coordinate( x / laenge, y / laenge );
    final GeometryFactory factory = new GeometryFactory( vector.getPrecisionModel(), vector.getSRID() );
    return factory.createPoint( coord );
  }

  /**
   * This function creates a line segment with the two given points, calculates the length of the line segment and
   * returns the length.
   * 
   * @param pointOne
   *            This point will be used as start point of the line segment.
   * @param pointTwo
   *            This point will be used as end point of the line segment.
   * @return The length of the line between the two points given.
   */
  public static double getLengthBetweenPoints( final Point pointOne, final Point pointTwo )
  {
    return getLengthBetweenPoints( pointOne.getCoordinate(), pointTwo.getCoordinate() );
  }

  /**
   * This function creates a line segment with the two given coordinates, calculates the length of the line segment and
   * returns the length.
   * 
   * @param coordinateOne
   *            This coordinate will be used as start point of the line segment.
   * @param coordinateTwo
   *            This coordinate will be used as end point of the line segment.
   * @return The length of the line between the two coordinates given.
   */
  public static double getLengthBetweenPoints( final Coordinate coordinateOne, final Coordinate coordinateTwo )
  {
    final LineSegment segment = new LineSegment( coordinateOne, coordinateTwo );
    return segment.getLength();
  }

  /** Creates a jts-polygon from a deegree-envelope */
  public static Polygon convertGMEnvelopeToPolygon( final GM_Envelope envelope, final GeometryFactory gf )
  {
    final Coordinate minCoord = JTSAdapter.export( envelope.getMin() );
    final Coordinate maxCoord = JTSAdapter.export( envelope.getMax() );
    final Coordinate tmp1Coord = new Coordinate( minCoord.x, maxCoord.y );
    final Coordinate tmp2Coord = new Coordinate( maxCoord.x, minCoord.y );

    final Coordinate[] coordinates = new Coordinate[] { minCoord, tmp1Coord, maxCoord, tmp2Coord, minCoord };
    final LinearRing linearRing = gf.createLinearRing( coordinates );
    return gf.createPolygon( linearRing, null );
  }

  /**
   * TODO: move to helper class Given 3 coordinate this methode return the equation of a plan containing those points.
   * The return equation as the form: z = Q*x+P*y+O The coefficients Q, P amd O are return as array
   * 
   * @param coords
   *            coordinate of 3 plane points
   * @return the cooeficients of the plane equation z = Q*x+P*y+O as array of double {Q,P,O}
   */
  public static final double[] calculateTrianglePlaneEquation( final Coordinate[] coords )
  {
    Assert.isNotNull( coords, "coords" );
    Assert.isTrue( coords.length >= 3, "Param coord which represent the point of a triangle must have a minimum length of 3" );

    final double x1 = coords[0].x;
    final double y1 = coords[0].y;
    final double z1 = coords[0].z;

    final double x2 = coords[1].x;
    final double y2 = coords[1].y;
    final double z2 = coords[1].z;

    final double x3 = coords[2].x;
    final double y3 = coords[2].y;
    final double z3 = coords[2].z;
    if( z1 == z2 && z2 == z3 )
      // z=-A/Cx-B/Cy-D/C = Q*x+P*y+O
      return new double[] { 0, 0, z1 };
    else
    {
      // build the equation Ax + By + Cz - D = 0
      double A = y1 * (z2 - z3) + y2 * (z3 - z1) + y3 * (z1 - z2);
      double B = z1 * (x2 - x3) + z2 * (x3 - x1) + z3 * (x1 - x2);
      final double C = x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2);
      final double D = x1 * (y2 * z3 - y3 * z2) + x2 * (y3 * z1 - y1 * z3) + x3 * (y1 * z2 - y2 * z1);

      // C=-C;
      // z=-A/Cx-B/Cy-D/C = Q*x+P*y+O
      return new double[] { -A / C, -B / C, D / C };
    }
  }

  /**
   * @param planarEquation
   *            Previously obtained by {@link #calculateTrianglePlaneEquation(Coordinate[])}. If <code>null</code>,
   *            <code>Double.NaN</code> will be returned.
   */
  public static double calculateTriangleZ( final double[] planeEquation, final double x, final double y )
  {
    if( planeEquation == null )
      return Double.NaN;
    return planeEquation[0] * x + planeEquation[1] * y + planeEquation[2];
  }

  /**
   * @param ring
   *            array of ordered coordinates, last must equal first one
   * @return signed area, area >= 0 means points are counter clockwise defined (mathematic positive) TODO: move it to
   *         JTSUtilities
   */
  public static double calcSignedAreaOfRing( final Coordinate[] ring )
  {
    if( ring.length < 4 ) // 3 points and 4. is repetition of first point
      throw new UnsupportedOperationException( "can not calculate area of < 3 points" );
    final Coordinate a = ring[0]; // base
    double area = 0;
    for( int i = 1; i < ring.length - 2; i++ )
    {
      final Coordinate b = ring[i];
      final Coordinate c = ring[i + 1];
      area += (b.y - a.y) * (a.x - c.x) // bounding rectangle

          - ((a.x - b.x) * (b.y - a.y)//
              + (b.x - c.x) * (b.y - c.y)//
          + (a.x - c.x) * (c.y - a.y)//
          ) / 2d;
    }
    return area;
  }

}