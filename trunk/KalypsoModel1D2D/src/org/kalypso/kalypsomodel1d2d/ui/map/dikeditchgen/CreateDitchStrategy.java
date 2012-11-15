/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
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
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.apache.commons.math3.util.Pair;
import org.kalypso.jts.JTSUtilities;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.elevation.IElevationModel;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.densify.Densifier;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateFilter;
import com.vividsolutions.jts.geom.CoordinateList;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineSegment;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.util.GeometryEditor;
import com.vividsolutions.jts.noding.SegmentString;
import com.vividsolutions.jts.simplify.DouglasPeuckerSimplifier;

/**
 * @author kurzbach
 */
public class CreateDitchStrategy implements CreateStructuredNetworkStrategy
{

  private final Geometry m_network;

  private final double m_innerWidthFraction;

  final double m_minimumDepth;

  final IElevationModel m_elevationModel;

  public CreateDitchStrategy( final Geometry network, final double innerWidthFraction, final double minimumDepth, final IElevationModel elevationModel )
  {
    m_network = network;
    m_innerWidthFraction = innerWidthFraction;
    m_minimumDepth = minimumDepth;
    m_elevationModel = elevationModel;
  }

  @Override
  public void addBoundary( final TriangulationBuilder tinBuilder ) throws GM_Exception
  {
    final String coordinateSystem = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    final int networkSize = m_network.getNumGeometries();
    // build raw (intersecting) buffer lines
    final Collection<SegmentString> bufferSegStrList = new ArrayList<>( networkSize );
    final double densifyFactor = m_innerWidthFraction > 0.5 ? 2 / m_innerWidthFraction : m_innerWidthFraction * 4;
    double clusterTolerance = Double.MAX_VALUE;
    for( int i = 0; i < networkSize; i++ )
    {
      final LineString linestring = getLine( i, densifyFactor );
      final Pair<Double, Double> userData = (Pair<Double, Double>)linestring.getUserData();
      final double startWidth = userData.getKey();
      final double endWidth = userData.getValue();
      final double minWidth = Math.min( startWidth, endWidth );
      clusterTolerance = minWidth < clusterTolerance ? minWidth : clusterTolerance;
      LineStringBufferBuilder.addRawBufferLines( linestring, startWidth, endWidth, startWidth, endWidth, bufferSegStrList );
    }
    final Polygon outerDensified = (Polygon)LineStringBufferBuilder.buffer( bufferSegStrList );
    final Polygon filtered = clusterPoints( outerDensified, clusterTolerance / 2 );
    filtered.apply( new InterpolateElevationFilter( coordinateSystem, m_elevationModel ) );
    final GM_Polygon outerRing = (GM_Polygon)JTSAdapter.wrap( filtered, coordinateSystem );
    tinBuilder.setBoundary( outerRing, false );
  }

  private Polygon clusterPoints( final Polygon polygon, final double clusterTolerance )
  {
    final LinearRing exteriorRing = (LinearRing)polygon.getExteriorRing();
    final LinearRing newExteriorRing = clusterPoints( exteriorRing, clusterTolerance );
    final int ringCount = polygon.getNumInteriorRing();
    final LinearRing[] newInteriorRings = new LinearRing[ringCount];
    for( int i = 0; i < ringCount; i++ )
      newInteriorRings[i] = clusterPoints( (LinearRing)polygon.getInteriorRingN( i ), clusterTolerance );
    return JTSAdapter.jtsFactory.createPolygon( newExteriorRing, newInteriorRings );
  }

  @SuppressWarnings( "unchecked" )
  private <T extends LineString> T clusterPoints( final T linestring, final double clusterTolerance )
  {
    final GeometryEditor editor = new GeometryEditor();
    final Geometry result = editor.edit( linestring, new GeometryEditor.CoordinateOperation()
    {

      @Override
      public Coordinate[] edit( final Coordinate[] coordinates, final Geometry geometry )
      {
        final CoordinateList coordList = new CoordinateList();
        coordList.add( coordinates[0] );
        for( int i = 1; i < coordinates.length - 1; i++ )
        {
          final Coordinate p0 = coordList.getCoordinate( coordList.size() - 1 );
          final Coordinate p1 = coordinates[i];
          final Coordinate p2 = coordinates[i + 1];
          if( (p0.distance( p1 ) <= clusterTolerance || p1.distance( p2 ) <= clusterTolerance) && new LineSegment( p0, p2 ).distance( p1 ) < clusterTolerance / 6 )
            continue;
          else
            coordList.add( p1 );
        }
        coordList.add( coordinates[coordinates.length - 1] );
        return coordList.toCoordinateArray();
      }
    } );
    return (T)result;
  }

  private LineString getLine( int i, final double densifyFactor )
  {
    final Geometry linestring = m_network.getGeometryN( i );
    final Pair<Double, Double> userData = (Pair<Double, Double>)linestring.getUserData();
    final double startWidth = userData.getKey();
    final double endWidth = userData.getValue();
    final double maxWidth = Math.max( startWidth, endWidth );
    final LineString simplified = (LineString)DouglasPeuckerSimplifier.simplify( linestring, maxWidth / 1000 );
    final LineString result = (LineString)Densifier.densify( simplified, maxWidth * densifyFactor );
    result.setUserData( linestring.getUserData() );
    return result;
  }

  @Override
  public void addBreaklines( final TriangulationBuilder tinBuilder ) throws GM_Exception
  {
    final String coordinateSystem = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    final int networkSize = m_network.getNumGeometries();

    // temporary elevation model for the outside
    final double minAngle = tinBuilder.getMinAngle();
    tinBuilder.setMinAngle( 0, false );
    tinBuilder.setNoSteiner( true );
    tinBuilder.finish();
    final GM_TriangulatedSurface tin = tinBuilder.getTin();
    tinBuilder.setMinAngle( minAngle, false );
    tinBuilder.setNoSteiner( false );

    final CoordinateFilter minimumDepthFilter = new CoordinateFilter()
    {

      @Override
      public void filter( final Coordinate coord )
      {
        try
        {
          final GM_Point p = GeometryFactory.createGM_Point( JTSAdapter.wrap( coord ), coordinateSystem );
          final double outsideElevation = tin.getValue( p );
          double depth = outsideElevation - coord.z;
          if( depth < m_minimumDepth )
            depth = m_minimumDepth;
          coord.z = outsideElevation - depth;
        }
        catch( final Exception e )
        {
          e.printStackTrace();
        }
      }
    };

    // build raw (intersecting) buffer lines
    final Collection<SegmentString> bufferSegStrList = new ArrayList<>( networkSize );
    final double densifyFactor = m_innerWidthFraction > 0.5 ? 4 / m_innerWidthFraction : m_innerWidthFraction * 2;
    double clusterTolerance = Double.MAX_VALUE;
    for( int i = 0; i < networkSize; i++ )
    {
      final LineString linestring = getLine( i, densifyFactor );
      final Pair<Double, Double> userData = (Pair<Double, Double>)linestring.getUserData();
      final double startWidth = userData.getKey() * m_innerWidthFraction;
      final double endWidth = userData.getValue() * m_innerWidthFraction;
      final double minWidth = Math.max( startWidth, endWidth );
      clusterTolerance = minWidth < clusterTolerance ? minWidth : clusterTolerance;
      final LineString linestringZ = JTSUtilities.interpolateMissingZ( linestring );
      linestringZ.apply( minimumDepthFilter );
      final GM_Curve edgeCurve = (GM_Curve)JTSAdapter.wrap( linestringZ, coordinateSystem );
      tinBuilder.addBreakLine( edgeCurve, false );
      LineStringBufferBuilder.addRawBufferLines( linestringZ, startWidth, endWidth, startWidth, endWidth, bufferSegStrList );
    }
    final Polygon innerDensified = (Polygon)LineStringBufferBuilder.buffer( bufferSegStrList );
    final Polygon filtered = clusterPoints( innerDensified, clusterTolerance / 2 );
    final List<Coordinate> networkCoordinates = Arrays.asList( m_network.getCoordinates() );

    // exterior ring
    final LineString exteriorRing = (LineString)JTSUtilities.addZCoordinates( filtered.getExteriorRing(), networkCoordinates, 2 );
    exteriorRing.apply( minimumDepthFilter );
    // interpolation does not work here, because the buffering process drops all z coordinates :-(
    // final LineString exteriorRing = JTSUtilities.interpolateMissingZ( innerDensified.getExteriorRing() );
    final GM_Curve exteriorRingAsCurve = (GM_Curve)JTSAdapter.wrap( exteriorRing, coordinateSystem );
    tinBuilder.addBreakLine( exteriorRingAsCurve, false );

    // interior rings
    final int ringCount = filtered.getNumInteriorRing();
    for( int i = 0; i < ringCount; i++ )
    {
      final LineString interiorRing = (LineString)JTSUtilities.addZCoordinates( filtered.getInteriorRingN( i ), networkCoordinates, 2 );
      interiorRing.apply( minimumDepthFilter );
      final GM_Curve innerRingAsCurve = (GM_Curve)JTSAdapter.wrap( interiorRing, coordinateSystem );
      tinBuilder.addBreakLine( innerRingAsCurve, false );
    }
  }
}
