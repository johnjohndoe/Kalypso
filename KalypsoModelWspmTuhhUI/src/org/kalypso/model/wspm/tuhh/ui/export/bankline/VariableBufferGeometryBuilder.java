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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import com.vividsolutions.jts.algorithm.LineIntersector;
import com.vividsolutions.jts.algorithm.RobustLineIntersector;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Location;
import com.vividsolutions.jts.geom.PrecisionModel;
import com.vividsolutions.jts.geom.TopologyException;
import com.vividsolutions.jts.geomgraph.Edge;
import com.vividsolutions.jts.geomgraph.EdgeList;
import com.vividsolutions.jts.geomgraph.Label;
import com.vividsolutions.jts.geomgraph.Node;
import com.vividsolutions.jts.geomgraph.PlanarGraph;
import com.vividsolutions.jts.geomgraph.Position;
import com.vividsolutions.jts.noding.IntersectionAdder;
import com.vividsolutions.jts.noding.MCIndexNoder;
import com.vividsolutions.jts.noding.NodedSegmentString;
import com.vividsolutions.jts.noding.Noder;
import com.vividsolutions.jts.noding.ScaledNoder;
import com.vividsolutions.jts.noding.SegmentString;
import com.vividsolutions.jts.noding.snapround.MCIndexSnapRounder;
import com.vividsolutions.jts.operation.buffer.BufferSubgraph;
import com.vividsolutions.jts.operation.buffer.SubgraphDepthLocater;
import com.vividsolutions.jts.operation.overlay.OverlayNodeFactory;
import com.vividsolutions.jts.operation.overlay.PolygonBuilder;

/**
 * @author Gernot Belger
 */
public class VariableBufferGeometryBuilder
{
  /**
   * A number of digits of precision which leaves some computational "headroom" for floating point operations. This
   * value should be less than the decimal precision of double-precision values (16).
   */
  private static int MAX_PRECISION_DIGITS = 12;

  private final EdgeList m_edgeList = new EdgeList();

  private final List<SegmentString> m_curveList = new ArrayList<>();

  private final GeometryFactory m_factory;

  private RuntimeException saveException; // debugging only

  private final Geometry m_originalGeometry;

  private final double m_maximalDistance;

  public VariableBufferGeometryBuilder( final Geometry originalGeometry, final double maximalDistance, final GeometryFactory factory )
  {
    m_originalGeometry = originalGeometry;
    m_maximalDistance = maximalDistance;
    m_factory = factory;
  }

  public Geometry computeGeometry( )
  {
    final Geometry resultGeometry = bufferOriginalPrecision();
    if( resultGeometry != null )
      return resultGeometry;

    final PrecisionModel argPM = m_factory.getPrecisionModel();
    if( argPM.getType() == PrecisionModel.FIXED )
      return bufferFixedPrecision( argPM );
    else
      return bufferReducedPrecision();
  }

  private Geometry bufferReducedPrecision( )
  {
    // try and compute with decreasing precision
    for( int precDigits = MAX_PRECISION_DIGITS; precDigits >= 0; precDigits-- )
    {
      try
      {
        bufferReducedPrecision( precDigits );
      }
      catch( final TopologyException ex )
      {
        // update the saved exception to reflect the new input geometry
        saveException = ex;
        // don't propagate the exception - it will be detected by fact that resultGeometry is null
      }
    }

    // tried everything - have to bail
    throw saveException;
  }

  private Geometry bufferOriginalPrecision( )
  {
    try
    {
      final PrecisionModel pm = m_factory.getPrecisionModel();

      // otherwise use a fast (but non-robust) noder
      final MCIndexNoder noder = new MCIndexNoder();
      final LineIntersector li = new RobustLineIntersector();
      li.setPrecisionModel( pm );
      noder.setSegmentIntersector( new IntersectionAdder( li ) );
// Noder noder = new IteratedNoder(precisionModel);
// Noder noder = new SimpleSnapRounder(precisionModel);
// Noder noder = new MCIndexSnapRounder(precisionModel);
// Noder noder = new ScaledNoder(new MCIndexSnapRounder(new PrecisionModel(1.0)),
// precisionModel.getScale());

      return createBufferGeometry( noder );
    }
    catch( final RuntimeException ex )
    {
      saveException = ex;
      // don't propagate the exception - it will be detected by fact that resultGeometry is null

      // testing ONLY - propagate exception
      // throw ex;
      return null;
    }
  }

  private void bufferReducedPrecision( final int precisionDigits )
  {
    final double sizeBasedScaleFactor = precisionScaleFactor( m_originalGeometry, m_maximalDistance, precisionDigits );
    // System.out.println("recomputing with precision scale factor = " + sizeBasedScaleFactor);

    final PrecisionModel fixedPM = new PrecisionModel( sizeBasedScaleFactor );
    bufferFixedPrecision( fixedPM );
  }

  private Geometry bufferFixedPrecision( final PrecisionModel fixedPM )
  {
    final Noder noder = new ScaledNoder( new MCIndexSnapRounder( new PrecisionModel( 1.0 ) ), fixedPM.getScale() );

    return createBufferGeometry( noder );
  }

  /**
   * Creates a {@link SegmentString} for a coordinate list which is a raw offset curve, and adds it to the list of
   * buffer curves. The SegmentString is tagged with a Label giving the topology of the curve. The curve may be oriented
   * in either direction. If the curve is oriented CW, the locations will be: <br>
   * Left: Location.EXTERIOR <br>
   * Right: Location.INTERIOR
   */
  public void addCurve( final Coordinate[] coord, final int leftLoc, final int rightLoc )
  {
    // don't add null or trivial curves
    if( coord == null || coord.length < 2 )
      return;
    // add the edge for a coordinate list which is a raw offset curve
    final SegmentString e = new NodedSegmentString( coord, new Label( 0, Location.BOUNDARY, leftLoc, rightLoc ) );
    m_curveList.add( e );
  }

  private Geometry createBufferGeometry( final Noder noder )
  {
    // short-circuit test
    if( m_curveList.size() <= 0 )
      return createEmptyResultGeometry();

    // for debugging: turn off noding
    final boolean noNoding = false;
    if( noNoding )
      return fromSegments();
    else
    {
      computeNodedEdges( noder, m_curveList );

      final PlanarGraph graph = new PlanarGraph( new OverlayNodeFactory() );
      graph.addEdges( m_edgeList.getEdges() );

      final List<BufferSubgraph> subgraphList = createSubgraphs( graph );
      final PolygonBuilder polyBuilder = new PolygonBuilder( m_factory );
      buildSubgraphs( subgraphList, polyBuilder );

      final List<Geometry> resultPolyList = polyBuilder.getPolygons();

      // just in case...
      if( resultPolyList.size() <= 0 )
        return createEmptyResultGeometry();

      return m_factory.buildGeometry( resultPolyList );
    }
  }

  private Geometry fromSegments( )
  {
    final Collection<Coordinate> allCrds = new ArrayList<>();

    for( final SegmentString segment : m_curveList )
    {
      final Coordinate[] coordinates = segment.getCoordinates();
      allCrds.addAll( Arrays.asList( coordinates ) );
    }

    final LinearRing ring = m_factory.createLinearRing( allCrds.toArray( new Coordinate[allCrds.size()] ) );

    return m_factory.createPolygon( ring, null );
  }

  /**
   * Gets the standard result for an empty buffer. Since buffer always returns a polygonal result, this is chosen to be
   * an empty polygon.
   *
   * @return the empty result geometry
   */
  private Geometry createEmptyResultGeometry( )
  {
    final Geometry emptyGeom = m_factory.createPolygon( null, null );
    return emptyGeom;
  }

  private void computeNodedEdges( final Noder noder, final List<SegmentString> bufferSegStrList )
  {
    noder.computeNodes( bufferSegStrList );

    final Collection<SegmentString> nodedSegStrings = noder.getNodedSubstrings();

    for( final SegmentString segmentString : nodedSegStrings )
    {
      final SegmentString segStr = segmentString;
      final Label oldLabel = (Label) segStr.getData();

      final Coordinate[] coordinates = segStr.getCoordinates();
      if( coordinates.length < 2 )
      {
        System.out.println( "oups" ); //$NON-NLS-1$
      }
      else
      {
        final Edge edge = new Edge( coordinates, new Label( oldLabel ) );
        insertUniqueEdge( edge );
      }
    }
  }

  /**
   * Inserted edges are checked to see if an identical edge already exists. If so, the edge is not inserted, but its
   * label is merged with the existing edge.
   */
  protected void insertUniqueEdge( final Edge e )
  {
// <FIX> MD 8 Oct 03 speed up identical edge lookup
    // fast lookup
    final Edge existingEdge = m_edgeList.findEqualEdge( e );

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
      m_edgeList.add( e );
      e.setDepthDelta( depthDelta( e.getLabel() ) );
    }
  }

  /**
   * Compute the change in depth as an edge is crossed from R to L
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

  private List<BufferSubgraph> createSubgraphs( final PlanarGraph graph )
  {
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
    /**
     * Sort the subgraphs in descending order of their rightmost coordinate. This ensures that when the Polygons for the
     * subgraphs are built, subgraphs for shells will have been built before the subgraphs for any holes they contain.
     */
    Collections.sort( subgraphList, Collections.reverseOrder() );
    return subgraphList;
  }

  /**
   * Completes the building of the input subgraphs by depth-labelling them, and adds them to the PolygonBuilder. The
   * subgraph list must be sorted in rightmost-coordinate order.
   *
   * @param subgraphList
   *          the subgraphs to build
   * @param polyBuilder
   *          the PolygonBuilder which will build the final polygons
   */
  private void buildSubgraphs( final List<BufferSubgraph> subgraphList, final PolygonBuilder polyBuilder )
  {
    final List<BufferSubgraph> processedGraphs = new ArrayList<>();
    for( final BufferSubgraph bufferSubgraph : subgraphList )
    {
      final BufferSubgraph subgraph = bufferSubgraph;
      final Coordinate p = subgraph.getRightmostCoordinate();
// int outsideDepth = 0;
// if (polyBuilder.containsPoint(p))
// outsideDepth = 1;
      final SubgraphDepthLocater locater = new SubgraphDepthLocater( processedGraphs );
      final int outsideDepth = locater.getDepth( p );
// try {
      subgraph.computeDepth( outsideDepth );
// }
// catch (RuntimeException ex) {
// // debugging only
// //subgraph.saveDirEdges();
// throw ex;
// }
      subgraph.findResultEdges();
      processedGraphs.add( subgraph );
      polyBuilder.add( subgraph.getDirectedEdges(), subgraph.getNodes() );
    }
  }

  /**
   * Compute a scale factor to limit the precision of a given combination of Geometry and buffer distance. The scale
   * factor is determined by a combination of the number of digits of precision in the (geometry + buffer distance),
   * limited by the supplied <code>maxPrecisionDigits</code> value.
   *
   * @param g
   *          the Geometry being buffered
   * @param distance
   *          the buffer distance
   * @param maxPrecisionDigits
   *          the max # of digits that should be allowed by the precision determined by the computed scale factor
   * @return a scale factor for the buffer computation
   */
  private static double precisionScaleFactor( final Geometry g, final double distance, final int maxPrecisionDigits )
  {
    final Envelope env = g.getEnvelopeInternal();
    final double envSize = Math.max( env.getHeight(), env.getWidth() );
    final double expandByDistance = distance > 0.0 ? distance : 0.0;
    final double bufEnvSize = envSize + 2 * expandByDistance;

    // the smallest power of 10 greater than the buffer envelope
    final int bufEnvLog10 = (int) (Math.log( bufEnvSize ) / Math.log( 10 ) + 1.0);
    final int minUnitLog10 = bufEnvLog10 - maxPrecisionDigits;
    // scale factor is inverse of min Unit size, so flip sign of exponent
    final double scaleFactor = Math.pow( 10.0, -minUnitLog10 );
    return scaleFactor;
  }
}
