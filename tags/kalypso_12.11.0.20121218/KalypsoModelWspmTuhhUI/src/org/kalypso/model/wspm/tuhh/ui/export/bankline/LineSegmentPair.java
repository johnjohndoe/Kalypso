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

import org.kalypso.jts.JtsVectorUtilities;

import com.vividsolutions.jts.algorithm.Angle;
import com.vividsolutions.jts.algorithm.CGAlgorithms;
import com.vividsolutions.jts.algorithm.HCoordinate;
import com.vividsolutions.jts.algorithm.LineIntersector;
import com.vividsolutions.jts.algorithm.NotRepresentableException;
import com.vividsolutions.jts.algorithm.RobustLineIntersector;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineSegment;
import com.vividsolutions.jts.geomgraph.Position;
import com.vividsolutions.jts.operation.buffer.BufferParameters;

/**
 * @author Gernot Belger
 */
class LineSegmentPair
{
  /**
   * Factor which controls how close offset segments can be to skip adding a filler or mitre.
   */
  private static final double OFFSET_SEGMENT_SEPARATION_FACTOR = 1.0E-3;

  /**
   * Factor which controls how close curve vertices on inside turns can be to be snapped
   */
  private static final double INSIDE_TURN_VERTEX_SNAP_DISTANCE_FACTOR = 1.0E-3;

  /**
   * Factor which determines how short closing segs can be for round buffers
   */
  private static final int MAX_CLOSING_SEG_LEN_FACTOR = 80;

  private final LineIntersector m_lineIntersector = new RobustLineIntersector();

  private LineSegment m_segment0;

  private LineSegment m_segment1;

  private LineSegment m_buffer0;

  private LineSegment m_buffer1;

  /* station at start of segment1 */
  private double m_length1 = 0.0;

  /* station at end of segment1 */
  private double m_length2 = 0.0;

  /* distance at start of segment1 */
  private double m_distance1 = Double.NaN;

  /* distance at end of segment1 */
  private double m_distance2 = Double.NaN;

  private final double m_tolerance;

  private final VariableOffsetCurveBuilder m_bufferBuilder;

  private final int m_side;

  private final BufferParameters m_bufferParams;

  private final double m_filletAngleQuantum;

  /**
   * The Closing Segment Length Factor controls how long "closing segments" are. Closing segments are added at the
   * middle of inside corners to ensure a smoother boundary for the buffer offset curve. In some cases (particularly for
   * round joins with default-or-better quantization) the closing segments can be made quite short. This substantially
   * improves performance (due to fewer intersections being created). A closingSegFactor of 0 results in lines to the
   * corner vertex A closingSegFactor of 1 results in lines halfway to the corner vertex A closingSegFactor of 80
   * results in lines 1/81 of the way to the corner vertex (this option is reasonable for the very common default
   * situation of round joins and quadrantSegs >= 8)
   */
  private final int m_closingSegLengthFactor;

  public LineSegmentPair( final VariableOffsetCurveBuilder bufferBuilder, final double tolerance, final BufferParameters bufferParams )
  {
    m_bufferBuilder = bufferBuilder;
    m_tolerance = tolerance;
    m_bufferParams = bufferParams;

    m_filletAngleQuantum = Math.PI / 2.0 / bufferParams.getQuadrantSegments();

    /**
     * Non-round joins cause issues with short closing segments, so don't use them. In any case, non-round joins only
     * really make sense for relatively small buffer distances.
     */
    if( m_bufferParams.getQuadrantSegments() >= 8 && m_bufferParams.getJoinStyle() == BufferParameters.JOIN_ROUND )
      m_closingSegLengthFactor = MAX_CLOSING_SEG_LEN_FACTOR;
    else
      m_closingSegLengthFactor = 1;

    m_side = bufferBuilder.getDistanceSignum() > 0 ? Position.LEFT : Position.RIGHT;
  }

  public void pushSegment( final LineSegment segment )
  {
    final double length = segment.getLength();

    /* update station of segment points */
    m_length1 = m_length2;
    m_length2 = m_length1 + length;

    /* update distance at station */
    // REMARK: NaN check needed for very first segment
    m_distance1 = Double.isNaN( m_distance2 ) ? m_bufferBuilder.getDistance( m_length1 ) : m_distance2;
    m_distance2 = m_bufferBuilder.getDistance( m_length2 );

    /*
     * Only push the segment, if length is greater than the tolerance, the distance is always updated however, in order
     * to keep the correct stationing of the line
     */
    if( length > m_tolerance )
    {
      m_segment0 = m_segment1;
      m_segment1 = segment;

      m_buffer0 = m_buffer1;
      m_buffer1 = calculateBuffer( m_segment1, m_distance1, m_distance2 );
    }

    addBufferCoordinates();
  }

  private LineSegment calculateBuffer( final LineSegment segment, final double distanceStart, final double distanceEnd )
  {
    final Coordinate bufferStart = JtsVectorUtilities.getOrthogonalPoint( segment, segment.p0, distanceStart );
    final Coordinate bufferEnd = JtsVectorUtilities.getOrthogonalPoint( segment, segment.p1, distanceEnd );

    return new LineSegment( bufferStart, bufferEnd );
  }

  private void addBufferCoordinates( )
  {
    if( m_segment0 == null || m_segment1 == null || m_buffer0 == null || m_buffer1 == null )
      return;

    final int orientation = CGAlgorithms.computeOrientation( m_segment0.p0, m_segment0.p1, m_segment1.p1 );
    final boolean outsideTurn = (orientation == CGAlgorithms.CLOCKWISE && m_side == Position.LEFT) || (orientation == CGAlgorithms.COUNTERCLOCKWISE && m_side == Position.RIGHT);

    // REMARK: we add only points at the current buffer vertex (i.e. the common point of the two segments);
    // the very first and very last vertex must be added separately
    // last crd of last segment is added separately

    final boolean addStartPoint = true;

    if( orientation == 0 )
    {
      addCollinear( addStartPoint );
    }
    else if( outsideTurn )
    {
      addOutsideTurn( orientation, addStartPoint );
    }
    else
    {
      addInsideTurn();
    }
  }

  private void addCollinear( final boolean addStartPoint )
  {
    /**
     * This test could probably be done more efficiently, but the situation of exact collinearity should be fairly rare.
     */
    m_lineIntersector.computeIntersection( m_segment0.p0, m_segment0.p1, m_segment1.p0, m_segment1.p1 );

    final int numInt = m_lineIntersector.getIntersectionNum();
    /**
     * if numInt is < 2, the lines are parallel and in the same direction. In this case the point can be ignored, since
     * the offset lines will also be parallel.
     */
    if( numInt >= 2 )
    {
      /**
       * segments are collinear but reversing. Add an "end-cap" fillet all the way around to other direction This case
       * should ONLY happen for LineStrings, so the orientation is always CW. (Polygons can never have two consecutive
       * segments which are parallel but reversed, because that would be a self intersection.
       */
      if( m_bufferParams.getJoinStyle() == BufferParameters.JOIN_BEVEL || m_bufferParams.getJoinStyle() == BufferParameters.JOIN_MITRE )
      {
        if( addStartPoint )
          m_bufferBuilder.addVertices( m_buffer0.p1 );
        m_bufferBuilder.addVertices( m_buffer1.p0 );
      }
      else
      {
        addFillet( m_segment0.p1, m_buffer0.p1, m_buffer1.p0, CGAlgorithms.CLOCKWISE, m_distance1 );
      }
    }
  }

  /**
   * Adds the offset points for an outside (convex) turn
   *
   * @param orientation
   * @param addStartPoint
   */
  private void addOutsideTurn( final int orientation, final boolean addStartPoint )
  {
    /**
     * Heuristic: If offset endpoints are very close together, just use one of them as the corner vertex. This avoids
     * problems with computing mitre corners in the case where the two segments are almost parallel (which is hard to
     * compute a robust intersection for).
     */
    if( m_buffer0.p1.distance( m_buffer1.p0 ) < m_distance1 * OFFSET_SEGMENT_SEPARATION_FACTOR )
    {
      m_bufferBuilder.addVertices( m_buffer0.p1 );
      return;
    }

    if( m_bufferParams.getJoinStyle() == BufferParameters.JOIN_MITRE )
    {
      addMitreJoin( m_segment0.p1, m_buffer0, m_buffer1, m_distance1 );
    }
    else if( m_bufferParams.getJoinStyle() == BufferParameters.JOIN_BEVEL )
    {
      addBevelJoin( m_buffer0, m_buffer1 );
    }
    else
    {
      // add a circular fillet connecting the endpoints of the offset segments
      if( addStartPoint )
        m_bufferBuilder.addVertices( m_buffer0.p1 );
      // TESTING - comment out to produce beveled joins
      addFillet( m_segment0.p1, m_buffer0.p1, m_buffer1.p0, orientation, Math.abs( m_distance1 ) );
      m_bufferBuilder.addVertices( m_buffer1.p0 );
    }
  }

  /**
   * Adds the offset points for an inside (concave) turn.
   *
   * @param orientation
   * @param addStartPoint
   */
  private void addInsideTurn( )
  {
    /**
     * add intersection point of offset segments (if any)
     */
    m_lineIntersector.computeIntersection( m_buffer0.p0, m_buffer0.p1, m_buffer1.p0, m_buffer1.p1 );
    if( m_lineIntersector.hasIntersection() )
    {
      m_bufferBuilder.addVertices( m_lineIntersector.getIntersection( 0 ) );
    }
    else
    {
      /**
       * If no intersection is detected, it means the angle is so small and/or the offset so large that the offsets
       * segments don't intersect. In this case we must add a "closing segment" to make sure the buffer curve is
       * continuous, fairly smooth (e.g. no sharp reversals in direction) and tracks the buffer correctly around the
       * corner. The curve connects the endpoints of the segment offsets to points which lie toward the centre point of
       * the corner. The joining curve will not appear in the final buffer outline, since it is completely internal to
       * the buffer polygon. In complex buffer cases the closing segment may cut across many other segments in the
       * generated offset curve. In order to improve the performance of the noding, the closing segment should be kept
       * as short as possible. (But not too short, since that would defeat its purpose). This is the purpose of the
       * closingSegFactor heuristic value.
       */

      /**
       * The intersection test above is vulnerable to robustness errors; i.e. it may be that the offsets should
       * intersect very close to their endpoints, but aren't reported as such due to rounding. To handle this situation
       * appropriately, we use the following test: If the offset points are very close, don't add closing segments but
       * simply use one of the offset points
       */
      // hasNarrowConcaveAngle = true;
      // System.out.println("NARROW ANGLE - distance = " + distance);
      if( m_buffer0.p1.distance( m_buffer1.p0 ) < m_distance1 * INSIDE_TURN_VERTEX_SNAP_DISTANCE_FACTOR )
      {
        m_bufferBuilder.addVertices( m_buffer0.p1 );
      }
      else
      {
        // add endpoint of this segment offset
        m_bufferBuilder.addVertices( m_buffer0.p1 );

        /**
         * Add "closing segment" of required length.
         */
        if( m_closingSegLengthFactor > 0 )
        {
          final Coordinate s1 = m_segment0.p1;

          final Coordinate mid0 = new Coordinate( (m_closingSegLengthFactor * m_buffer0.p1.x + s1.x) / (m_closingSegLengthFactor + 1), (m_closingSegLengthFactor * m_buffer0.p1.y + s1.y)
              / (m_closingSegLengthFactor + 1) );
          m_bufferBuilder.addVertices( mid0 );
          final Coordinate mid1 = new Coordinate( (m_closingSegLengthFactor * m_buffer1.p0.x + s1.x) / (m_closingSegLengthFactor + 1), (m_closingSegLengthFactor * m_buffer1.p0.y + s1.y)
              / (m_closingSegLengthFactor + 1) );
          m_bufferBuilder.addVertices( mid1 );
        }
        else
        {
          /**
           * This branch is not expected to be used except for testing purposes. It is equivalent to the JTS 1.9 logic
           * for closing segments (which results in very poor performance for large buffer distances)
           */
          m_bufferBuilder.addVertices( m_segment0.p1 );
        }

        // */
        // add start point of next segment offset
        m_bufferBuilder.addVertices( m_buffer1.p0 );
      }
    }
  }

  /**
   * Add points for a circular fillet around a reflex corner. Adds the start and end points
   *
   * @param p
   *          base point of curve
   * @param p0
   *          start point of fillet curve
   * @param p1
   *          endpoint of fillet curve
   * @param direction
   *          the orientation of the fillet
   * @param radius
   *          the radius of the fillet
   */
  private void addFillet( final Coordinate p, final Coordinate p0, final Coordinate p1, final int direction, final double radius )
  {
    final double dx0 = p0.x - p.x;
    final double dy0 = p0.y - p.y;
    double startAngle = Math.atan2( dy0, dx0 );
    final double dx1 = p1.x - p.x;
    final double dy1 = p1.y - p.y;
    final double endAngle = Math.atan2( dy1, dx1 );

    if( direction == CGAlgorithms.CLOCKWISE )
    {
      if( startAngle <= endAngle )
        startAngle += 2.0 * Math.PI;
    }
    else
    { // direction == COUNTERCLOCKWISE
      if( startAngle >= endAngle )
        startAngle -= 2.0 * Math.PI;
    }
    m_bufferBuilder.addVertices( p0 );
    addFillet( p, startAngle, endAngle, direction, radius );
    m_bufferBuilder.addVertices( p1 );
  }

  /**
   * Adds points for a circular fillet arc between two specified angles. The start and end point for the fillet are not
   * added - the caller must add them if required.
   *
   * @param direction
   *          is -1 for a CW angle, 1 for a CCW angle
   * @param radius
   *          the radius of the fillet
   */
  private void addFillet( final Coordinate p, final double startAngle, final double endAngle, final int direction, final double radius )
  {
    final int directionFactor = direction == CGAlgorithms.CLOCKWISE ? -1 : 1;

    final double totalAngle = Math.abs( startAngle - endAngle );
    final int nSegs = (int) (totalAngle / m_filletAngleQuantum + 0.5);

    if( nSegs < 1 )
      return; // no segments because angle is less than increment - nothing to do!

    double initAngle, currAngleInc;

    // choose angle increment so that each segment has equal length
    initAngle = 0.0;
    currAngleInc = totalAngle / nSegs;

    double currAngle = initAngle;
    final Coordinate pt = new Coordinate();
    while( currAngle < totalAngle )
    {
      final double angle = startAngle + directionFactor * currAngle;
      pt.x = p.x + radius * Math.cos( angle );
      pt.y = p.y + radius * Math.sin( angle );
      m_bufferBuilder.addVertices( pt );
      currAngle += currAngleInc;
    }
  }

  /**
   * Adds a mitre join connecting the two reflex offset segments. The mitre will be beveled if it exceeds the mitre
   * ratio limit.
   *
   * @param offset0
   *          the first offset segment
   * @param offset1
   *          the second offset segment
   * @param distance
   *          the offset distance
   */
  private void addMitreJoin( final Coordinate p, final LineSegment offset0, final LineSegment offset1, final double distance )
  {
    boolean isMitreWithinLimit = true;
    Coordinate intPt = null;

    /**
     * This computation is unstable if the offset segments are nearly collinear. Howver, this situation should have been
     * eliminated earlier by the check for whether the offset segment endpoints are almost coincident
     */
    try
    {
      intPt = HCoordinate.intersection( offset0.p0, offset0.p1, offset1.p0, offset1.p1 );

      final double mitreRatio = distance <= 0.0 ? 1.0 : intPt.distance( p ) / Math.abs( distance );

      if( mitreRatio > m_bufferParams.getMitreLimit() )
        isMitreWithinLimit = false;
    }
    catch( final NotRepresentableException ex )
    {
      intPt = new Coordinate( 0, 0 );
      isMitreWithinLimit = false;
    }

    if( isMitreWithinLimit )
    {
      m_bufferBuilder.addVertices( intPt );
    }
    else
    {
      addLimitedMitreJoin( distance, m_bufferParams.getMitreLimit() );
    }
  }

  /**
   * Adds a limited mitre join connecting the two reflex offset segments. A limited mitre is a mitre which is beveled at
   * the distance determined by the mitre ratio limit.
   *
   * @param offset0
   *          the first offset segment
   * @param offset1
   *          the second offset segment
   * @param distance
   *          the offset distance
   * @param mitreLimit
   *          the mitre limit ratio
   */
  private void addLimitedMitreJoin( final double distance, final double mitreLimit )
  {
    final Coordinate basePt = m_segment0.p1;

    final double ang0 = Angle.angle( basePt, m_segment0.p0 );

    // oriented angle between segments
    final double angDiff = Angle.angleBetweenOriented( m_segment0.p0, basePt, m_segment1.p1 );
    // half of the interior angle
    final double angDiffHalf = angDiff / 2;

    // angle for bisector of the interior angle between the segments
    final double midAng = Angle.normalize( ang0 + angDiffHalf );
    // rotating this by PI gives the bisector of the reflex angle
    final double mitreMidAng = Angle.normalize( midAng + Math.PI );

    // the miterLimit determines the distance to the mitre bevel
    final double mitreDist = mitreLimit * distance;
    // the bevel delta is the difference between the buffer distance
    // and half of the length of the bevel segment
    final double bevelDelta = mitreDist * Math.abs( Math.sin( angDiffHalf ) );
    final double bevelHalfLen = distance - bevelDelta;

    // compute the midpoint of the bevel segment
    final double bevelMidX = basePt.x + mitreDist * Math.cos( mitreMidAng );
    final double bevelMidY = basePt.y + mitreDist * Math.sin( mitreMidAng );
    final Coordinate bevelMidPt = new Coordinate( bevelMidX, bevelMidY );

    // compute the mitre midline segment from the corner point to the bevel segment midpoint
    final LineSegment mitreMidLine = new LineSegment( basePt, bevelMidPt );

    // finally the bevel segment endpoints are computed as offsets from
    // the mitre midline
    final Coordinate bevelEndLeft = mitreMidLine.pointAlongOffset( 1.0, bevelHalfLen );
    final Coordinate bevelEndRight = mitreMidLine.pointAlongOffset( 1.0, -bevelHalfLen );

    if( m_side == Position.LEFT )
    {
      m_bufferBuilder.addVertices( bevelEndLeft );
      m_bufferBuilder.addVertices( bevelEndRight );
    }
    else
    {
      m_bufferBuilder.addVertices( bevelEndRight );
      m_bufferBuilder.addVertices( bevelEndLeft );
    }
  }

  /**
   * Adds a bevel join connecting the two offset segments around a reflex corner.
   *
   * @param offset0
   *          the first offset segment
   * @param offset1
   *          the second offset segment
   */
  private void addBevelJoin( final LineSegment offset0, final LineSegment offset1 )
  {
    m_bufferBuilder.addVertices( offset0.p1 );
    m_bufferBuilder.addVertices( offset1.p0 );
  }

  public double getDistance2( )
  {
    return m_distance2;
  }
}