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
package org.kalypso.kalypsomodel1d2d.ui.map.dikeditchgen;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import com.google.common.collect.Lists;
import com.vividsolutions.jts.algorithm.CGAlgorithms;
import com.vividsolutions.jts.algorithm.LineIntersector;
import com.vividsolutions.jts.algorithm.RobustLineIntersector;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateList;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineSegment;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Location;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.PrecisionModel;
import com.vividsolutions.jts.geomgraph.Edge;
import com.vividsolutions.jts.geomgraph.EdgeList;
import com.vividsolutions.jts.geomgraph.Label;
import com.vividsolutions.jts.geomgraph.Node;
import com.vividsolutions.jts.geomgraph.PlanarGraph;
import com.vividsolutions.jts.geomgraph.Position;
import com.vividsolutions.jts.noding.IntersectionAdder;
import com.vividsolutions.jts.noding.MCIndexNoder;
import com.vividsolutions.jts.noding.NodedSegmentString;
import com.vividsolutions.jts.noding.SegmentString;
import com.vividsolutions.jts.operation.buffer.BufferSubgraph;
import com.vividsolutions.jts.operation.buffer.SubgraphDepthLocater;
import com.vividsolutions.jts.operation.overlay.OverlayNodeFactory;
import com.vividsolutions.jts.operation.overlay.PolygonBuilder;

/**
 * @author kurzbach
 */
@SuppressWarnings( "unchecked" )
public class LineStringBufferBuilder
{

  private static void addInsideTurn( final Coordinate p, final LineSegment seg0, final LineSegment seg1, final CoordinateList coordinates, final double tolerance )
  {
    final LineIntersector lineIntersector = new RobustLineIntersector();
    lineIntersector.computeIntersection( seg0.p0, seg0.p1, seg1.p0, seg1.p1 );
    if( lineIntersector.hasIntersection() )
    {
      final Coordinate intersection = lineIntersector.getIntersection( 0 );
      intersection.z = p.z;
      coordinates.add( intersection, false );
    }
    else if( seg0.p1.distance( seg1.p0 ) < tolerance )
    {
      final Coordinate mid = new Coordinate( (seg0.p1.x + seg1.p0.x) / 2, (seg0.p1.y + seg1.p0.y) / 2, (seg0.p1.z + seg1.p0.z) / 2 );
      coordinates.add( mid, false );
    }
    else
    {
      final Coordinate mid0 = new Coordinate( (seg0.p1.x + p.x) / 2, (seg0.p1.y + p.y) / 2, (seg0.p1.z + p.z) / 2 );
      coordinates.add( mid0 );
      final Coordinate mid1 = new Coordinate( (seg1.p0.x + p.x) / 2, (seg1.p0.y + p.y) / 2, (seg1.p0.z + p.z) / 2 );
      coordinates.add( mid1 );
    }
  }

  private static void addFillet( final Coordinate center, final double startAngle, final double endAngle, final int orientation, final double radius, final CoordinateList coordinates )
  {
    final int orientationFactor = orientation == CGAlgorithms.CLOCKWISE ? -1 : 1;

    final double totalAngle = Math.abs( startAngle - endAngle );
    final int nSegs = (int)(totalAngle * 4 / Math.PI + 0.5);

    if( nSegs < 1 )
      return; // no segments because angle is less than increment - nothing to do!

    double initAngle, currAngleInc;

    // choose angle increment so that each segment has equal length
    initAngle = 0.0;
    currAngleInc = totalAngle / nSegs;

    double currAngle = initAngle;
    while( currAngle < totalAngle )
    {
      final double angle = startAngle + orientationFactor * currAngle;
      final double x = center.x + radius * Math.cos( angle );
      final double y = center.y + radius * Math.sin( angle );
      final double z = center.z;
      coordinates.add( new Coordinate( x, y, z ), false );
      currAngle += currAngleInc;
    }
  }

  private static void computeLeftAndRightSegments( final LineSegment seg, final double startLeftWidth, final double endLeftWidth, final LineSegment leftSeg, final double startRightWidth, final double endRightWidth, final LineSegment rightSeg )
  {
    final double dx = seg.p1.x - seg.p0.x;
    final double dy = seg.p1.y - seg.p0.y;
    final double len = Math.sqrt( dx * dx + dy * dy );
    final double dxnorm = dx / len;
    final double dynorm = dy / len;

    final Coordinate startLeft = new Coordinate( seg.p0.x - dynorm * startLeftWidth, seg.p0.y + dxnorm * startLeftWidth, seg.p0.z );
    final Coordinate endLeft = new Coordinate( seg.p1.x - dynorm * endLeftWidth, seg.p1.y + dxnorm * endLeftWidth, seg.p1.z );
    leftSeg.p0 = startLeft;
    leftSeg.p1 = endLeft;

    // right segment
    final Coordinate startRight = new Coordinate( seg.p0.x + dynorm * startRightWidth, seg.p0.y - dxnorm * startRightWidth, seg.p0.z );
    final Coordinate endRight = new Coordinate( seg.p1.x + dynorm * endRightWidth, seg.p1.y - dxnorm * endRightWidth, seg.p1.z );
    rightSeg.p0 = startRight;
    rightSeg.p1 = endRight;
  }

  private static void addOutsideTurn( final Coordinate center, final Coordinate from, final Coordinate to, final int orientation, final CoordinateList coordinates, final double tolerance )
  {
    // start
    if( from.distance( to ) < tolerance )
    {
      coordinates.add( new Coordinate( (from.x + to.x) / 2, (from.y + to.y) / 2, (from.z + to.z) / 2 ), false );
      return;
    }
    coordinates.add( from, false );

    // fillet
    final double dx0 = from.x - center.x;
    final double dy0 = from.y - center.y;
    double startAngle = Math.atan2( dy0, dx0 );
    final double dx1 = to.x - center.x;
    final double dy1 = to.y - center.y;
    final double endAngle = Math.atan2( dy1, dx1 );
    if( orientation == CGAlgorithms.CLOCKWISE )
    {
      if( startAngle <= endAngle )
        startAngle += 2.0 * Math.PI;
    }
    else
    { // orientation == COUNTERCLOCKWISE
      if( startAngle >= endAngle )
        startAngle -= 2.0 * Math.PI;
    }
    addFillet( center, startAngle, endAngle, orientation, center.distance( to ), coordinates );

    // end
    coordinates.add( to, false );
  }

  public static void addRawBufferLines( final LineString linestring, final double startLeftWidth, final double endLeftWidth, final double startRightWidth, final double endRightWidth, final Collection<SegmentString> bufferSegStrList )
  {
    final double totalLength = linestring.getLength();
    final int numPoints = linestring.getNumPoints();
    final boolean closeRing = linestring.getStartPoint().distance( linestring.getEndPoint() ) < 0.01;

    final double leftWidthDiff = endLeftWidth - startLeftWidth;
    final double rightWidthDiff = endRightWidth - startRightWidth;

    // left and right coordinates go in parallel, i.e. opposite order
//    final int initialCapacity = numPoints * 2 + 6;
    final CoordinateList leftCoordinates = new CoordinateList();
    final CoordinateList rightCoordinates = new CoordinateList();

//    final LineSegment first = new LineSegment( new Coordinate( 0, 0 ), new Coordinate( 0, 1 ) );
    final LineSegment first = new LineSegment( linestring.getCoordinateN( 0 ), linestring.getCoordinateN( 1 ) );
    final LineSegment firstLeft = new LineSegment();
    final LineSegment firstRight = new LineSegment();
    final double firstFracLength = first.getLength();
    final double firstFrac = firstFracLength / totalLength;
    computeLeftAndRightSegments( first, startLeftWidth, startLeftWidth + leftWidthDiff * firstFrac, firstLeft, startRightWidth, startRightWidth + rightWidthDiff * firstFrac, firstRight );

    if( !closeRing )
      // make start tip
      addTip( first.p0, firstRight.p0, firstLeft.p0, CGAlgorithms.CLOCKWISE, leftCoordinates );

    final LineSegment seg01 = new LineSegment();
    final LineSegment leftSeg01 = new LineSegment();
    final LineSegment rightSeg01 = new LineSegment();
    seg01.p0 = first.p0;
    seg01.p1 = first.p1;
    double fracLength0 = 0;
    double fracLength1 = firstFracLength;
    for( int i = 2; i < numPoints; i++ )
    {
      final Coordinate nextCoordinate = linestring.getCoordinateN( i );

      // compute the left and right offset segments
      final double leftWidth0 = startLeftWidth + leftWidthDiff * fracLength0 / totalLength;
      final double leftWidth1 = startLeftWidth + leftWidthDiff * fracLength1 / totalLength;
      final double rightWidth0 = startRightWidth + rightWidthDiff * fracLength0 / totalLength;
      final double rightWidth1 = startRightWidth + rightWidthDiff * fracLength1 / totalLength;

      // for the segment p0-p1
      computeLeftAndRightSegments( seg01, leftWidth0, leftWidth1, leftSeg01, rightWidth0, rightWidth1, rightSeg01 );

      // for the segment p1-p2
      final LineSegment seg12 = new LineSegment( seg01.p1, nextCoordinate );
      final LineSegment leftSeg12 = new LineSegment();
      final LineSegment rightSeg12 = new LineSegment();
      final double fracLength2 = fracLength1 + seg12.getLength();
      final double leftWidth2 = startLeftWidth + leftWidthDiff * fracLength2 / totalLength;
      final double rightWidth2 = startRightWidth + rightWidthDiff * fracLength2 / totalLength;
      computeLeftAndRightSegments( seg12, leftWidth1, leftWidth2, leftSeg12, rightWidth1, rightWidth2, rightSeg12 );

      addTurn( seg01, leftSeg01, rightSeg01, seg12, leftSeg12, rightSeg12, leftCoordinates, rightCoordinates );

      seg01.p0 = seg12.p0;
      seg01.p1 = seg12.p1;
      fracLength0 = fracLength1;
      fracLength1 = fracLength2;
    }

    // make end tip or turn
    computeLeftAndRightSegments( seg01, startLeftWidth + leftWidthDiff * fracLength0 / totalLength, endLeftWidth, leftSeg01, endRightWidth + rightWidthDiff * fracLength0 / totalLength, endRightWidth, rightSeg01 );
    if( closeRing )
    {
      addTurn( seg01, leftSeg01, rightSeg01, first, firstLeft, firstRight, leftCoordinates, rightCoordinates );

      if( !leftCoordinates.get( 0 ).equals( leftCoordinates.get( leftCoordinates.size() - 1 ) ) )
        leftCoordinates.closeRing();

      final NodedSegmentString left = new NodedSegmentString( leftCoordinates.toCoordinateArray(), new Label( Location.BOUNDARY, Location.EXTERIOR, Location.INTERIOR ) );
      bufferSegStrList.add( left );

      if( !rightCoordinates.get( 0 ).equals( rightCoordinates.get( rightCoordinates.size() - 1 ) ) )
        rightCoordinates.closeRing();

      final NodedSegmentString right = new NodedSegmentString( rightCoordinates.toCoordinateArray(), new Label( Location.BOUNDARY, Location.INTERIOR, Location.EXTERIOR ) );
      bufferSegStrList.add( right );
    }
    else
    {
      addTip( seg01.p1, rightSeg01.p1, leftSeg01.p1, CGAlgorithms.COUNTERCLOCKWISE, rightCoordinates );

      // we have to flip the right side coordinate list
      leftCoordinates.addAll( Lists.reverse( rightCoordinates ), false );

      if( !leftCoordinates.get( 0 ).equals( leftCoordinates.get( leftCoordinates.size() - 1 ) ) )
        leftCoordinates.closeRing();

      final NodedSegmentString result = new NodedSegmentString( leftCoordinates.toCoordinateArray(), new Label( Location.BOUNDARY, Location.EXTERIOR, Location.INTERIOR ) );
      bufferSegStrList.add( result );
    }
  }

  private static void addTurn( final LineSegment seg01, final LineSegment leftSeg01, final LineSegment rightSeg01, final LineSegment seg12, final LineSegment leftSeg12, final LineSegment rightSeg12, final CoordinateList leftCoordinates, final CoordinateList rightCoordinates )
  {
    final int orientation = seg01.orientationIndex( seg12.p1 );
    final boolean outsideTurn = (orientation == CGAlgorithms.CLOCKWISE);
    if( orientation == 0 )
    {
      // co-linear segments seg01, seg12
      // can savely be ignored?
      // System.out.println( "colinear" );
      leftCoordinates.add( leftSeg01.p1, false );
      rightCoordinates.add( rightSeg01.p1, false );
    }
    final double tolerance = leftSeg01.distance( rightSeg01 ) / 3;
    if( outsideTurn )
    {
      // outside turn on left side
      addOutsideTurn( seg01.p1, leftSeg01.p1, leftSeg12.p0, CGAlgorithms.CLOCKWISE, leftCoordinates, tolerance );
      // inside turn on right side
      addInsideTurn( seg01.p1, rightSeg01, rightSeg12, rightCoordinates, tolerance );
    }
    else
    {
      // inside turn on left side
      addInsideTurn( seg01.p1, leftSeg01, leftSeg12, leftCoordinates, tolerance );
      // outside turn on right side
      addOutsideTurn( seg01.p1, rightSeg01.p1, rightSeg12.p0, CGAlgorithms.COUNTERCLOCKWISE, rightCoordinates, tolerance );
    }
  }

  /**
   * COPIED FROM {@link com.vividsolutions.jts.operation.buffer.BufferBuilder} Compute the change in depth as an edge is crossed from R to L
   */
  private static int depthDelta( final Label label )
  {
    final int lLoc = label.getLocation( 0, Position.LEFT );
    final int rLoc = label.getLocation( 0, Position.RIGHT );
    if( lLoc == Location.INTERIOR && rLoc == Location.EXTERIOR )
      return 1;
    else if( lLoc == Location.EXTERIOR && rLoc == Location.INTERIOR )
      return -1;
    return 0;
  }

  private static void addTip( final Coordinate center, final Coordinate from, final Coordinate to, final int orientation, final CoordinateList coordinates )
  {
    final int orientationFactor = orientation == CGAlgorithms.CLOCKWISE ? -1 : 1;

    // vector center->from
    final double dx0 = from.x - center.x;
    final double dy0 = from.y - center.y;
    final double fromRadius = Math.sqrt( dx0 * dx0 + dy0 * dy0 );
    // vector to->center
    final double dx1 = center.x - to.x;
    final double dy1 = center.y - to.y;
    final double toRadius = Math.sqrt( dx1 * dx1 + dy1 * dy1 );

    // make sure that from is the start point
    if( fromRadius > toRadius )
    {
      final CoordinateList tipCoords = new CoordinateList();
      addTip( center, to, from, orientation * -1, tipCoords );
      coordinates.addAll( Lists.reverse( tipCoords ), false );
      return;
    }

    // start
    coordinates.add( from, false );

    // make a 180 degree turn
    final double startAngle = Math.atan2( dy0, dx0 );
    final int numHalfCircleFractions = 6;
    final double sectionAngle = Math.PI / numHalfCircleFractions;
    for( int i = 1; i < numHalfCircleFractions; i++ )
    {
      final double angleFrom = startAngle + orientationFactor * sectionAngle * i;
      final double x = center.x + fromRadius * Math.cos( angleFrom );
      final double y = center.y + fromRadius * Math.sin( angleFrom );
      final double z = center.z;
      coordinates.add( new Coordinate( x, y, z ), false );
    }

    // end
    coordinates.add( to, false );
  }

  public static void addRawBufferLines( final LineString linestring, final double leftWidth, final double rightWidth, final Collection<SegmentString> bufferSegStrList )
  {
    addRawBufferLines( linestring, leftWidth, leftWidth, rightWidth, rightWidth, bufferSegStrList );
  }

  public static Geometry buffer( final LineString linestring, final double startLeftWidth, final double endLeftWidth, final double startRightWidth, final double endRightWidth )
  {
    final Collection<SegmentString> bufferSegStrList = new ArrayList<>( 1 );
    addRawBufferLines( linestring, startLeftWidth, endLeftWidth, startRightWidth, endRightWidth, bufferSegStrList );
    final Geometry resultGeom = buffer( bufferSegStrList );
    return resultGeom;
  }

  public static Geometry buffer( final Geometry linearGeom, final double leftWidth, final double rightWidth )
  {
    // build raw (intersecting) buffer lines
    final int networkSize = linearGeom.getNumGeometries();
    final Collection<SegmentString> bufferSegStrList = new ArrayList<>( networkSize );
    for( int i = 0; i < networkSize; i++ )
    {
      final LineString linestring = (LineString)linearGeom.getGeometryN( i );
      addRawBufferLines( linestring, leftWidth, rightWidth, bufferSegStrList );
    }
    final Geometry resultGeom = buffer( bufferSegStrList );
    return resultGeom;
  }

  public static Geometry buffer( final Collection<SegmentString> bufferSegStrList )
  {
    // node the raw buffer lines
    final com.vividsolutions.jts.geom.GeometryFactory geomFact = new GeometryFactory( new PrecisionModel( 1000 ) );
    final MCIndexNoder noder = new MCIndexNoder();
    final LineIntersector li = new RobustLineIntersector();
//    li.setPrecisionModel( geomFact.getPrecisionModel() );
    noder.setSegmentIntersector( new IntersectionAdder( li ) );
    noder.computeNodes( bufferSegStrList );
    final Collection<SegmentString> nodedSegStrings = noder.getNodedSubstrings();

    // build edge list
    final EdgeList edgeList = new EdgeList();
    for( final Iterator<SegmentString> i = nodedSegStrings.iterator(); i.hasNext(); )
    {
      final SegmentString segStr = i.next();
      final Label oldLabel = (Label)segStr.getData();

      final Coordinate[] coordinates = segStr.getCoordinates();
      if( coordinates.length < 2 )
        continue;

      final Edge e = new Edge( coordinates, new Label( oldLabel ) );
      final Edge existingEdge = edgeList.findEqualEdge( e );

      // If an identical edge already exists, simply update its label
      if( existingEdge != null )
      {
        final Label existingLabel = existingEdge.getLabel();

        Label labelToMerge = e.getLabel();
        // check if new edge is in reverse direction to existing edge
        // if so, must flip the label before merging it
        if( !existingEdge.isPointwiseEqual( e ) )
        {
          labelToMerge = new Label( e.getLabel() );
          labelToMerge.flip();
        }
        existingLabel.merge( labelToMerge );

        // compute new depth delta of sum of edges
        final int mergeDelta = depthDelta( labelToMerge );
        final int existingDelta = existingEdge.getDepthDelta();
        final int newDelta = existingDelta + mergeDelta;
        existingEdge.setDepthDelta( newDelta );
      }
      else
      { // no matching existing edge was found
        // add this new edge to the list of edges in this graph
        // e.setName(name + edges.size());
        edgeList.add( e );
        e.setDepthDelta( depthDelta( e.getLabel() ) );
      }
    }

    final PlanarGraph graph = new PlanarGraph( new OverlayNodeFactory() );
    graph.addEdges( edgeList.getEdges() );

    final List<BufferSubgraph> subgraphList = new ArrayList<>();
    for( final Iterator<Node> i = graph.getNodes().iterator(); i.hasNext(); )
    {
      final Node node = i.next();
      if( !node.isVisited() )
      {
        final BufferSubgraph subgraph = new BufferSubgraph();
        subgraph.create( node );
        subgraphList.add( subgraph );
      }
    }
    Collections.sort( subgraphList, Collections.reverseOrder() );

    final PolygonBuilder polyBuilder = new PolygonBuilder( geomFact );
    final List<BufferSubgraph> processedGraphs = new ArrayList<>();
    for( final Iterator<BufferSubgraph> i = subgraphList.iterator(); i.hasNext(); )
    {
      final BufferSubgraph subgraph = i.next();
      final Coordinate p = subgraph.getRightmostCoordinate();
      final SubgraphDepthLocater locater = new SubgraphDepthLocater( processedGraphs );
      final int outsideDepth = locater.getDepth( p );
      subgraph.computeDepth( outsideDepth );
      subgraph.findResultEdges();
      processedGraphs.add( subgraph );
      polyBuilder.add( subgraph.getDirectedEdges(), subgraph.getNodes() );
    }

    final List<Polygon> resultPolyList = polyBuilder.getPolygons();
    final Geometry resultGeom = geomFact.buildGeometry( resultPolyList );
    return resultGeom;
  }

}
