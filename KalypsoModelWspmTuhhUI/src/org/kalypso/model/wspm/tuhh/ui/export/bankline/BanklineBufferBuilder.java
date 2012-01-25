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

import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.SortedMap;

import org.kalypso.commons.math.geom.PolyLine;
import org.kalypso.jts.JtsVectorUtilities;

import com.vividsolutions.jts.algorithm.CGAlgorithms;
import com.vividsolutions.jts.algorithm.LineIntersector;
import com.vividsolutions.jts.algorithm.RobustLineIntersector;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateList;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineSegment;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.noding.IntersectionAdder;
import com.vividsolutions.jts.noding.MCIndexNoder;
import com.vividsolutions.jts.noding.NodedSegmentString;
import com.vividsolutions.jts.noding.Noder;
import com.vividsolutions.jts.noding.SegmentString;

/**
 * @author Gernot Belger
 */
public class BanklineBufferBuilder
{
  private final Collection<Polygon> m_segmentAreas = new ArrayList<>();

  private final CoordinateList m_bufferVertices = new CoordinateList();

  private final SortedMap<Double, BanklineDistances> m_distances;

  private final String m_name;

  private final LineString m_line;

  private PolyLine m_distancer;

  private final double m_distanceSignum;

  private final GeometryFactory m_factory = new GeometryFactory();

  public BanklineBufferBuilder( final SortedMap<Double, BanklineDistances> distances, final String name, final LineString line, final double distanceSignum )
  {
    m_distances = distances;
    m_name = name;
    m_line = line;
    m_distanceSignum = distanceSignum;
  }

  public Geometry buffer( )
  {
    initDistancer();

    /* If distancer was not initialized, we have not enough distance values, we can only return the empty collection now */
    if( m_distancer == null )
      return m_factory.createGeometryCollection( null );

    doBuffering();

// return buildBufferPolygon();
    return buildBufferLine();
  }

  private void initDistancer( )
  {
    final Collection<BanklineDistances> values = m_distances.values();
    final BanklineDistances[] distances = values.toArray( new BanklineDistances[values.size()] );

    final Collection<Point2D> points = new ArrayList<>();

    for( final BanklineDistances dis : distances )
    {
      final double x = dis.getStation();
      final double y = dis.getDistance( m_name );
      if( !Double.isNaN( y ) )
        points.add( new Point2D.Double( x, y ) );
    }

    final Point2D[] allPoints = points.toArray( new Point2D[points.size()] );
    if( allPoints.length > 1 )
      m_distancer = new PolyLine( allPoints, 0.0001 );
  }

  private void doBuffering( )
  {
    // REMARK: strange: without simplification, the result is totally broken...
// final Geometry simplifiedLine = DouglasPeuckerSimplifier.simplify( m_line, 0.0001 );

// final Coordinate[] coordinates = BufferInputLineSimplifier.simplify( simplifiedLine.getCoordinates(), 0.01 );
// final Coordinate[] coordinates = simplifiedLine.getCoordinates();
    final Coordinate[] coordinates = m_line.getCoordinates();

    /* Add first vertex */
    if( coordinates.length > 0 )
    {
      final LineSegment firstSegment = new LineSegment( coordinates[0], coordinates[1] );
      final Coordinate firstVertex = JtsVectorUtilities.getOrthogonalPoint( firstSegment, firstSegment.p0, getDistance( 0.0 ) );
      addVertices( firstVertex );
    }

    double length = 0.0;

    for( int i = 1; i < coordinates.length - 1; i++ )
    {
      final LineSegment segment0 = new LineSegment( coordinates[i - 1], coordinates[i] );
      final LineSegment segment1 = new LineSegment( coordinates[i], coordinates[i + 1] );

      final double distance0 = getDistance( length );
      final double distance1 = getDistance( length + segment0.getLength() );
      final double distance2 = getDistance( length + segment0.getLength() + segment1.getLength() );

      addSegmentAsPatch( segment0, segment1, distance1, distance2 );
      addSegmentAsLine( segment0, segment1, distance0, distance1, distance2 );

      length += segment0.getLength();
    }

    /* Add last vertex */
    if( coordinates.length > 1 )
    {
      final LineSegment lastSegment = new LineSegment( coordinates[coordinates.length - 2], coordinates[coordinates.length - 1] );
      final Coordinate lastVertex = JtsVectorUtilities.getOrthogonalPoint( lastSegment, lastSegment.p1, getDistance( m_line.getLength() ) );
      addVertices( lastVertex );
    }
  }

  private Geometry buildBufferPolygon( )
  {
    final Polygon[] allPolygons = m_segmentAreas.toArray( new Polygon[m_segmentAreas.size()] );

    final GeometryCollection collection = m_factory.createGeometryCollection( allPolygons );

    try
    {
      final boolean buildUnion = false;
      if( buildUnion )
        return collection.union();
      else
        return collection;
    }
    catch( final Exception e )
    {
      // REMARK: sometimes the 'union' does not work (JTS bug), but sequentially unions does, so we try this here...
      e.printStackTrace();
      return buildContinuousUnion( collection );
    }
  }

  private Geometry buildContinuousUnion( final GeometryCollection collection )
  {
    Geometry continousUnion = collection.getGeometryN( 0 );

    final Collection<Geometry> problematic = new ArrayList<>();

    for( int i = 1; i < collection.getNumGeometries(); i++ )
    {
      try
      {
        continousUnion = continousUnion.union( collection.getGeometryN( i ) );
      }
      catch( final Exception e )
      {
        e.printStackTrace();

        problematic.add( collection );
      }
    }

    final Geometry[] result = problematic.toArray( new Geometry[problematic.size() + 1] );
    result[problematic.size()] = continousUnion;
    return collection.getFactory().createGeometryCollection( result );
  }

  private LineString buildBufferLine( )
  {
    /* Element self intersection in inner curves */
    final SegmentString segments = new NodedSegmentString( m_bufferVertices.toCoordinateArray(), null );
    final Collection<SegmentString> bufferSegments = Collections.singletonList( segments );

    final Noder noder = createNoder();
    noder.computeNodes( bufferSegments );
    final Collection<SegmentString> nodedSegments = noder.getNodedSubstrings();

    final CoordinateList nodedCoordinates = new CoordinateList();

    for( final Iterator<SegmentString> iterator = nodedSegments.iterator(); iterator.hasNext(); )
    {
      final NodedSegmentString nodedSegment = (NodedSegmentString) iterator.next();

      final Coordinate[] coordinates = nodedSegment.getCoordinates();
      nodedCoordinates.add( coordinates, false );

      /* Skip all odd nodes; they are the parts within the intersection */
      if( iterator.hasNext() )
        iterator.next();
    }

    return m_factory.createLineString( nodedCoordinates.toCoordinateArray() );
// return m_factory.createLineString( m_bufferVertices.toCoordinateArray() );
  }

  private Noder createNoder( )
  {
    final MCIndexNoder noder = new MCIndexNoder();
    final LineIntersector li = new RobustLineIntersector();
    li.setPrecisionModel( m_factory.getPrecisionModel() );
    noder.setSegmentIntersector( new IntersectionAdder( li ) );
    return noder;
  }

  private void addSegmentAsPatch( final LineSegment lastSegment, final LineSegment segment, final double distance0, final double distance1 )
  {
    final Coordinate c0 = segment.p0;
    final Coordinate c1 = segment.p1;
    final Coordinate c2 = JtsVectorUtilities.getOrthogonalPoint( segment, c1, distance1 );
    final Coordinate c3 = JtsVectorUtilities.getOrthogonalPoint( segment, c0, distance0 );

    /* a trapezoid for the current segment */
    addPatch( new Coordinate[] { c0, c1, c2, c3, c0 } );

    /* a triangle for the gap between the two segments */
    if( lastSegment == null )
      return;

    /* a trapezoid for the previous segment */
    final Coordinate cX = JtsVectorUtilities.getOrthogonalPoint( lastSegment, lastSegment.p1, distance0 );
    addPatch( new Coordinate[] { c0, c3, cX, c0 } );
  }

  private void addPatch( final Coordinate[] coordinates )
  {
    final LinearRing shell = m_factory.createLinearRing( coordinates );

    final Polygon polygon = m_factory.createPolygon( shell, null );
    polygon.normalize();

    if( polygon.getArea() <= 0.0 )
      return;

    if( !polygon.isValid() )
      return;

    if( !polygon.isSimple() )
      return;

    m_segmentAreas.add( polygon );
  }

  private void addSegmentAsLine( final LineSegment segment0, final LineSegment segment1, final double distance0, final double distance1, final double distance2 )
  {
    if( segment0 == null )
      return;

    final Coordinate s0_start = JtsVectorUtilities.getOrthogonalPoint( segment0, segment0.p0, distance0 );
    final Coordinate s0_end = JtsVectorUtilities.getOrthogonalPoint( segment0, segment0.p1, distance1 );

    final Coordinate s1_start = JtsVectorUtilities.getOrthogonalPoint( segment1, segment1.p0, distance1 );
    final Coordinate s1_end = JtsVectorUtilities.getOrthogonalPoint( segment1, segment1.p1, distance2 );

    final LineSegment buffer0 = new LineSegment( s0_start, s0_end );
    final LineSegment buffer1 = new LineSegment( s1_start, s1_end );

    final int orientation = CGAlgorithms.computeOrientation( segment0.p0, segment0.p1, segment1.p1 );
    final boolean outsideTurn = (orientation == CGAlgorithms.CLOCKWISE && m_distanceSignum > 0) || (orientation == CGAlgorithms.COUNTERCLOCKWISE && m_distanceSignum < 0);

    // REMARK: we add only points at the current buffer vertex (i.e. the common point of the two segments);
    // the very first and very last vertex must be added separately
    // last crd of last segment is added separately

    if( orientation == 0 )
    {
      // both end point will be at same location, just add one of them
      addVertices( buffer0.p1 );
      addVertices( buffer1.p0 );
    }
    else if( outsideTurn )
    {
      addVertices( buffer0.p1, buffer1.p0 );
    }
    else
    {
      addInsideTurn( buffer0, buffer1 );
    }
  }

  /**
   * Adds the offset points for an inside (concave) turn.
   *
   * @param orientation
   * @param addStartPoint
   */
  private void addInsideTurn( final LineSegment offset0, final LineSegment offset1 )
  {
    final LineIntersector li = new RobustLineIntersector();

    /**
     * add intersection point of offset segments (if any)
     */
    li.computeIntersection( offset0.p0, offset0.p1, offset1.p0, offset1.p1 );
    if( li.hasIntersection() )
    {
      addVertices( li.getIntersection( 0 ) );
    }
    else
    {
      final Coordinate mid = new LineSegment( offset0.p1, offset1.p0 ).midPoint();
      addVertices( mid );
    }
  }

  private void addVertices( final Coordinate... vertices )
  {
    m_bufferVertices.add( vertices, false );
  }

  private double getDistance( final double station )
  {
    return m_distancer.getYFor( station ) * m_distanceSignum;
  }
}