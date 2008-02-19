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
  public static Point linePointInGeometry( LineString line, Geometry geometry_2nd )
  {
    int numPoints = line.getNumPoints();

    for( int i = 0; i < numPoints; i++ )
    {
      Point pointN = line.getPointN( i );

      if( geometry_2nd.contains( pointN ) )
      {
        GeometryFactory factory = new GeometryFactory( pointN.getPrecisionModel(), pointN.getSRID() );
        return factory.createPoint( new Coordinate( pointN.getCoordinate() ) );
      }
    }

    return null;
  }

  /**
   * This function calculates the distance from the start point to a point, lying on the line.
   * 
   * @param line
   *            The line.
   * @param point
   *            One point lying on the line.
   * @return The distance of the point on the line.
   */
  public static double pointDistanceOnLine( LineString line, Point point )
  {
    /* Check for intersection. */
    if( (point.distance( line ) >= 10E-08) )
      throw new IllegalStateException( "The point does not lie on the line ..." );

    /* The needed factory. */
    GeometryFactory factory = new GeometryFactory( line.getPrecisionModel(), line.getSRID() );

    /* Get all coordinates. */
    Coordinate[] coordinates = line.getCoordinates();

    /* Only loop until the one before the last one. */
    for( int i = 0; i < coordinates.length - 1; i++ )
    {
      /* Get the coordinates to the current one + 1. */
      Coordinate[] coords = (Coordinate[]) ArrayUtils.subarray( coordinates, 0, i + 2 );

      /* Create a new line with the coordinates. */
      LineString ls = factory.createLineString( coords );
      if( (point.distance( ls ) >= 10E-08) )
        continue;

      /* Point was intersecting the last segment, now take all coordinates but the last one ... */
      LinkedList<Coordinate> lineCoords = new LinkedList<Coordinate>();
      for( int j = 0; j < coords.length - 1; j++ )
        lineCoords.add( coords[j] );

      /* ... and add the point as last one. */
      lineCoords.add( point.getCoordinate() );

      /* Create the new geometry. */
      LineString tempLine = factory.createLineString( lineCoords.toArray( new Coordinate[] {} ) );

      return tempLine.getLength();
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
  public static Point pointOnLine( LineString lineJTS, double distance )
  {
    double length = lineJTS.getLength();

    if( distance < 0 || distance > length )
      return null;

    int numPoints = lineJTS.getNumPoints();

    if( numPoints == 0 )
      return null;

    /* Only loop until the point before the last point. */
    LineString line = null;
    for( int i = 0; i < numPoints - 1; i++ )
    {
      Point startPoint = lineJTS.getPointN( i );
      Point endPoint = lineJTS.getPointN( i + 1 );

      GeometryFactory factory = new GeometryFactory( lineJTS.getPrecisionModel(), lineJTS.getSRID() );
      line = factory.createLineString( new Coordinate[] { new Coordinate( startPoint.getCoordinate() ), new Coordinate( endPoint.getCoordinate() ) } );
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

      /* If the two Z koords are equal, take one of them for the new point. */
      double zStart = startPoint.getCoordinate().z;
      double zEnd = endPoint.getCoordinate().z;

      if( zStart != Double.NaN && zEnd != Double.NaN )
      {
        if( Double.compare( zStart, zEnd ) != 0 )
        {
          LinearEquation computeZ = new LinearEquation( zStart, 0, zEnd, max );
          zStart = computeZ.computeX( distance );
        }
      }
      else
        zStart = Double.NaN;

      GeometryFactory factory = new GeometryFactory( lineJTS.getPrecisionModel(), lineJTS.getSRID() );
      Point pointJTS = factory.createPoint( new Coordinate( x, y, zStart ) );

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
   *            The line string on which the point has to be.
   * @param percent
   *            The distance in percent at which the point should be placed on the line.
   * @return The newly created point on the line or null, if something was wrong. Update: returns the start point or the
   *         end point if percentage is 0 or 100.
   */
  public static Point pointOnLinePercent( LineString lineJTS, int percent )
  {
    if( percent < 0 || percent > 100 )
      return null;

    double length = lineJTS.getLength();
    double distance = length / 100.0 * percent;

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
  public static LineString createLineSegment( Geometry line, Point start, Point end )
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
  public static boolean getLineOrientation( LineString line, Point start, Point end )
  {
    /* Check if both points are lying on the line. */
    if( (line.distance( start ) >= 10E-08) || (line.distance( start ) >= 10E-08) )
      throw new IllegalArgumentException( "One of the two points does not lie on the given line ..." );

    boolean first = false;

    for( int i = 0; i < line.getNumPoints() - 1; i++ )
    {
      Point pointN = line.getPointN( i );
      Point pointN1 = line.getPointN( i + 1 );

      /* Build a line with the two points to check the flag. */
      LineSegment testLine = new LineSegment( new Coordinate( pointN.getCoordinate() ), new Coordinate( pointN1.getCoordinate() ) );

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
  private static LineString createLineSegmentFromLine( LineString line, Point start, Point end )
  {
    List<Point> points = new LinkedList<Point>();

    boolean add = false;

    points.add( start );

    for( int i = 0; i < line.getNumPoints() - 1; i++ )
    {
      Point pointN = line.getPointN( i );
      Point pointN1 = line.getPointN( i + 1 );

      if( add )
        points.add( pointN );

      /* Build a line with the two points to check the flag. */
      LineSegment testLine = new LineSegment( new Coordinate( pointN.getCoordinate() ), new Coordinate( pointN1.getCoordinate() ) );

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
    Coordinate[] coordinates = new Coordinate[points.size()];

    for( int i = 0; i < points.size(); i++ )
      coordinates[i] = new Coordinate( points.get( i ).getCoordinate() );

    GeometryFactory factory = new GeometryFactory( line.getPrecisionModel(), line.getSRID() );

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
  private static LineString createLineSegmentFromMultiLine( MultiLineString line, Point start, Point end )
  {
    List<Point> points = new LinkedList<Point>();

    boolean add = false;
    boolean endPointFound = false;

    points.add( start );
    for( int i = 0; i < line.getNumGeometries(); i++ )
    {
      LineString lineN = (LineString) line.getGeometryN( i );

      for( int j = 0; j < lineN.getNumPoints() - 1; j++ )
      {
        Point pointN = lineN.getPointN( j );
        Point pointN1 = lineN.getPointN( j + 1 );

        if( add )
          points.add( pointN );

        /* Build a line with the two points to check the flag. */
        LineSegment testLine = new LineSegment( new Coordinate( pointN.getCoordinate() ), new Coordinate( pointN1.getCoordinate() ) );

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
    Coordinate[] coordinates = new Coordinate[points.size()];

    for( int i = 0; i < points.size(); i++ )
      coordinates[i] = new Coordinate( points.get( i ).getCoordinate() );
    GeometryFactory factory = new GeometryFactory( line.getPrecisionModel(), line.getSRID() );

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
  public static Point getVector( Point start, Point end )
  {
    Coordinate coords = new Coordinate( start.getX() - end.getX(), start.getY() - end.getY() );
    GeometryFactory factory = new GeometryFactory( start.getPrecisionModel(), start.getSRID() );

    return factory.createPoint( coords );
  }

  /**
   * Calculates a normalized vector.
   * 
   * @param vector
   *            The vector to be normalized.
   * @return The normalized vector.
   */
  public static Point getNormalizedVector( Point vector )
  {
    double x = vector.getX();
    double y = vector.getY();

    /* The length of a vector is the sum of all elements with the power of two and than the square root of it. */
    double laenge = Math.sqrt( x * x + y * y );

    Coordinate coord = new Coordinate( x / laenge, y / laenge );
    GeometryFactory factory = new GeometryFactory( vector.getPrecisionModel(), vector.getSRID() );
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
  public static double getLengthBetweenPoints( Point pointOne, Point pointTwo )
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
  public static double getLengthBetweenPoints( Coordinate coordinateOne, Coordinate coordinateTwo )
  {
    LineSegment segment = new LineSegment( coordinateOne, coordinateTwo );
    return segment.getLength();
  }

  /** Creates a jts-polygon from a deegree-envelope */
  public static Polygon convertGMEnvelopeToPolygon( GM_Envelope envelope, GeometryFactory gf )
  {
    Coordinate minCoord = JTSAdapter.export( envelope.getMin() );
    Coordinate maxCoord = JTSAdapter.export( envelope.getMax() );
    Coordinate tmp1Coord = new Coordinate( minCoord.x, maxCoord.y );
    Coordinate tmp2Coord = new Coordinate( maxCoord.x, minCoord.y );

    Coordinate[] coordinates = new Coordinate[] { minCoord, tmp1Coord, maxCoord, tmp2Coord, minCoord };
    LinearRing linearRing = gf.createLinearRing( coordinates );
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
  public static double[] calculateTrianglePlaneEquation( Coordinate[] coords )
  {
    Assert.isNotNull( coords, "coords" );
    Assert.isTrue( coords.length >= 3, "Param coord which represent the point of a triangle must have a minimum length of 3" );

    double x1 = coords[0].x;
    double y1 = coords[0].y;
    double z1 = coords[0].z;

    double x2 = coords[1].x;
    double y2 = coords[1].y;
    double z2 = coords[1].z;

    double x3 = coords[2].x;
    double y3 = coords[2].y;
    double z3 = coords[2].z;
    if( z1 == z2 && z2 == z3 )
      // z=-A/Cx-B/Cy-D/C = Q*x+P*y+O
      return new double[] { 0, 0, z1 };
    else
    {
      // build the equation Ax + By + Cz - D = 0
      double A = y1 * (z2 - z3) + y2 * (z3 - z1) + y3 * (z1 - z2);
      double B = z1 * (x2 - x3) + z2 * (x3 - x1) + z3 * (x1 - x2);
      double C = x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2);
      double D = x1 * (y2 * z3 - y3 * z2) + x2 * (y3 * z1 - y1 * z3) + x3 * (y1 * z2 - y2 * z1);

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
  public static double calculateTriangleZ( double[] planeEquation, double x, double y )
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
  public static double calcSignedAreaOfRing( Coordinate[] ring )
  {
    if( ring.length < 4 ) // 3 points and 4. is repetition of first point
      throw new UnsupportedOperationException( "can not calculate area of < 3 points" );

    Coordinate a = ring[0]; // base
    double area = 0;
    for( int i = 1; i < ring.length - 2; i++ )
    {
      Coordinate b = ring[i];
      Coordinate c = ring[i + 1];

      area += (b.y - a.y) * (a.x - c.x) // bounding rectangle

          - ((a.x - b.x) * (b.y - a.y)//
              + (b.x - c.x) * (b.y - c.y)//
          + (a.x - c.x) * (c.y - a.y)//
          ) / 2d;
    }

    return area;
  }

  /**
   * This function will check all line segments and return the one, in which the given point lies. If no segment is
   * found it will return null.
   * 
   * @param curve
   *            The curve to check.
   * @param point
   *            The point, which marks the segment (e.g. an intersection point of another geometry).
   * @return The line segment or null.
   */
  public static LineSegment findLineSegment( LineString curve, Point point )
  {
    for( int i = 0; i < curve.getNumPoints() - 1; i++ )
    {
      Point pointN = curve.getPointN( i );
      Point pointN1 = curve.getPointN( i + 1 );

      /* Build a line with the two points to check the intersection. */
      LineSegment segment = new LineSegment( new Coordinate( pointN.getCoordinate() ), new Coordinate( pointN1.getCoordinate() ) );

      /* If found, return it. */
      if( segment.distance( point.getCoordinate() ) < 10E-08 )
        return segment;
    }

    return null;
  }

  /**
   * This function adds points to the line.
   * 
   * @param line
   *            The line, to which the points are added to.
   * @param points
   *            The points, which should be added. The points has to lie on the line.
   * @return The new line as copy of the old line, including the given points. The result may be null.
   */
  public static LineString addPointsToLine( LineString line, List<Point> points )
  {
    /* Check for intersection. */
    for( int i = 0; i < points.size(); i++ )
    {
      if( (points.get( i ).distance( line ) >= 10E-08) )
        throw new IllegalStateException( "One of the points does not lie on the line ..." );
    }

    /* The geometry factory. */
    GeometryFactory factory = new GeometryFactory( line.getPrecisionModel(), line.getSRID() );

    /* Memory for the new coordinates. */
    ArrayList<Coordinate> newCoordinates = new ArrayList<Coordinate>();

    /* Get all coordinates. */
    Coordinate[] lineCoordinates = line.getCoordinates();

    /* Always add the first coordinate. */
    newCoordinates.add( lineCoordinates[0] );

    /* Only loop until the one before the last one. */
    for( int i = 0; i < lineCoordinates.length - 1; i++ )
    {
      /* Get the coordinates. */
      Coordinate startCoord = lineCoordinates[i];
      Coordinate endCoord = lineCoordinates[i + 1];

      /* Create a new line with the coordinates. */
      LineString ls = factory.createLineString( new Coordinate[] { startCoord, endCoord } );

      /* If no one is intersecting, the current end coordinate has to be added. */
      ArrayList<Point> toRemove = new ArrayList<Point>();
      for( int j = 0; j < points.size(); j++ )
      {
        Point point = points.get( j );
        if( (point.distance( ls ) < 10E-08) )
        {
          /* The point intersects, and has to be added. */
          newCoordinates.add( point.getCoordinate() );

          /* The points should be removed from the old points list for perfomance reasons. */
          toRemove.add( point );
          continue;
        }

        /* The point does not intersect, check the next one. */
        continue;
      }

      /* No point was added and should be removed from the points list. */
      if( toRemove.size() > 0 )
      {
        /* Remove all added points. */
        points.removeAll( toRemove );
      }

      /* Add the end coordinate. */
      newCoordinates.add( endCoord );
    }

    /* Create the new geometry. */
    LineString newLine = factory.createLineString( newCoordinates.toArray( new Coordinate[] {} ) );

    return newLine;
  }

  /**
   * This function adds points every 1m to the geometry.
   * 
   * @param curve
   *            The curve, which represents the geometry on the map of the profile.
   * @return A new curve with the new points.
   */
  public static LineString addPointsToLine( LineString curve )
  {
    /* The length of the line. */
    double length = curve.getLength();

    /* Memory for the new points. */
    ArrayList<Point> points = new ArrayList<Point>();

    /* If there is only 1 meter, the start- and endpoint have to suffice. */
    double usedLength = 1.0;
    while( usedLength < length )
    {
      /* Create a new point. */
      Point pointOnLine = pointOnLine( curve, usedLength );

      /* Add the found point to the list. */
      points.add( pointOnLine );

      /* Increse the used length by 1 meter. */
      usedLength = usedLength + 1;
    }

    /* Add the points to the line. */
    /* ATTENTION: curve will be a new line, the original line is not modified! */
    curve = addPointsToLine( curve, points );

    return curve;
  }
}