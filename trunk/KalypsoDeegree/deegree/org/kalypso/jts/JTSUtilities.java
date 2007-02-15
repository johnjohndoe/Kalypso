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

import java.util.LinkedList;
import java.util.List;

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
   *          The points of this line will be checked. The first, which lies in the given geometry is returned.
   * @param geometry_2nd
   *          The points of the line will be checked with this geometry.
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

      GeometryFactory factory = new GeometryFactory( lineJTS.getPrecisionModel(), lineJTS.getSRID() );
      Point pointJTS = factory.createPoint( new Coordinate( x, y ) );

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
  public static Point pointOnLinePercent( LineString lineJTS, int percent )
  {
    if( percent < 0 || percent > 100 )
      return null;

    double length = lineJTS.getLength();
    double distance = length / 100.0 * percent;

    return pointOnLine( lineJTS, (int) distance );
  }

  /**
   * This function creates a line segment (JTS) of a line from a given start point to an end point, including all points
   * on the given line.
   * 
   * @param line
   *          The original line.
   * @param start
   *          The start point of the new line (it has to be one point that lies on the original line).
   * @param end
   *          The end point of the new line (it has to be one point that lies on the original line).
   */
  public static LineString createLineSegment( Geometry line, Point start, Point end )
  {
    // if( !line.crosses( start ) )
    // return null;
    //
    // if( !line.crosses( end ) )
    // return null;

    if( line instanceof LineString )
      return createLineSegmentFromLine( (LineString) line, start, end );
    else if( line instanceof MultiLineString )
      return createLineSegmentFromMultiLine( (MultiLineString) line, start, end );

    return null;
  }

  /**
   * Evaluates the two given points and returns true, if the direction is equal of that from line (its points).
   * 
   * @param line
   *          The original LineString.
   * @param start
   *          The start point of the new line (it has to be one point that lies on the original LineString).
   * @param end
   *          The end point of the new line (it has to be one point that lies on the original LineString).
   * @return True, if the first found point of the line is nearer to the start point, than to the end point.
   */
  public static boolean getLineOrientation( LineString line, Point start, Point end )
  {
    /* Check if both points are lying on the line. */
    if(( line.distance( start ) >= 10E-08 ) || ( line.distance( start ) >= 10E-08 ) ){
      throw new IllegalArgumentException("One of the two points does not lie on the given line ...");
    }
    
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
   * points on the given LineString.
   * 
   * @param line
   *          The original LineString.
   * @param start
   *          The start point of the new line (it has to be one point that lies on the original LineString).
   * @param end
   *          The end point of the new line (it has to be one point that lies on the original LineString).
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
   * it should contain more than one LineString of the original MultiLineString.<br>
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
   *          The original MultiLineString.
   * @param start
   *          The start point of the new line (it has to be one point on the original MultiLineString).
   * @param end
   *          The end point of the new line (it has to be one point on the original MultiLineString).
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
   *          The start point of the line.
   * @param end
   *          The end point of the line.
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
   *          The vector to be normalized.
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
   *          This point will be used as start point of the line segment.
   * @param pointTwo
   *          This point will be used as end point of the line segment.
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
   *          This coordinate will be used as start point of the line segment.
   * @param coordinateTwo
   *          This coordinate will be used as end point of the line segment.
   * @return The length of the line between the two coordinates given.
   */
  public static double getLengthBetweenPoints( Coordinate coordinateOne, Coordinate coordinateTwo )
  {
    LineSegment segment = new LineSegment( coordinateOne, coordinateTwo );
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
}