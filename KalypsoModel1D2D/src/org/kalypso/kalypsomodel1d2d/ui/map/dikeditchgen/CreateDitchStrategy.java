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
import java.util.Collection;

import org.apache.commons.math3.util.Pair;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModelSystem;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_PolygonPatch;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.densify.Densifier;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateFilter;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.simplify.DouglasPeuckerSimplifier;

/**
 * @author kurzbach
 */
public class CreateDitchStrategy implements CreateStructuredNetworkStrategy
{

  private final Geometry m_network;

  private final double m_innerWidthFraction;

  final double m_targetElevation;

  final double m_minimumDepth;

  final ITerrainElevationModelSystem m_elevationModel;

  private final double m_innerDensifyTolerance = 4;

  private final double m_minimumHeight;

  public CreateDitchStrategy( Geometry network, double innerWidthFraction, double targetElevation, double minimumDepth, boolean enforceMinimumHeight, ITerrainElevationModelSystem elevationModel )
  {
    m_network = network;
    m_innerWidthFraction = innerWidthFraction;
    m_targetElevation = targetElevation;
    m_minimumDepth = minimumDepth;
    m_elevationModel = elevationModel;
    if( enforceMinimumHeight )
      m_minimumHeight = m_targetElevation + m_minimumDepth;
    else
      m_minimumHeight = Double.MIN_VALUE;
  }

  @Override
  public void addBoundary( TriangulationBuilder tinBuilder ) throws GM_Exception
  {
    final String coordinateSystem = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    final int networkSize = m_network.getNumGeometries();
    final Collection<Geometry> polygons = new ArrayList<>( networkSize );
    for( int i = 0; i < networkSize; i++ )
    {
      final LineString linestring = (LineString)m_network.getGeometryN( i );
      final Pair<Double, Double> userData = (Pair<Double, Double>)linestring.getUserData();
      final double startWidth = userData.getKey();
      final double endWidth = userData.getValue();
      final Geometry buffer = LineStringBufferBuilder.buffer( linestring, startWidth, endWidth, startWidth, endWidth );
      polygons.add( buffer );
    }
    Geometry outer = JTSAdapter.jtsFactory.buildGeometry( polygons ).buffer( 0 );
    outer = DouglasPeuckerSimplifier.simplify( outer, m_innerDensifyTolerance / m_innerWidthFraction / 20 );
    final Polygon outerDensified = (Polygon)Densifier.densify( outer, m_innerDensifyTolerance / m_innerWidthFraction );
    outerDensified.apply( new InterpolateElevationFilter( coordinateSystem, m_minimumHeight, m_elevationModel ) );
    final GM_Polygon outerRing = (GM_Polygon)JTSAdapter.wrap( outerDensified, coordinateSystem );
    tinBuilder.setBoundary( outerRing, false );
  }

  @Override
  public void addBreaklines( TriangulationBuilder tinBuilder ) throws GM_Exception
  {
    final String coordinateSystem = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    final int networkSize = m_network.getNumGeometries();

    final CoordinateFilter minimumDepthFilter = new CoordinateFilter()
    {

      @Override
      public void filter( Coordinate coord )
      {
        try
        {
          final GM_Point p = GeometryFactory.createGM_Point( JTSAdapter.wrap( coord ), coordinateSystem );
          final double currentElevation = m_elevationModel.getElevation( p );
          double depth = currentElevation - m_targetElevation;
          if( depth < m_minimumDepth )
            depth = m_minimumDepth;
          coord.z = currentElevation - depth;
        }
        catch( final Exception e )
        {
          e.printStackTrace();
        }
      }
    };

    final Geometry densifiedNetwork = Densifier.densify( m_network, m_innerDensifyTolerance );
    densifiedNetwork.apply( minimumDepthFilter );
    for( int i = 0; i < densifiedNetwork.getNumGeometries(); i++ )
    {
      final LineString linestring = (LineString)densifiedNetwork.getGeometryN( i );
      final GM_Curve edgeCurve = (GM_Curve)JTSAdapter.wrap( linestring, coordinateSystem );
      tinBuilder.addBreakLine( edgeCurve, false );
    }

    final Collection<Geometry> polygons = new ArrayList<>( networkSize );
    for( int i = 0; i < networkSize; i++ )
    {
      final LineString linestring = (LineString)m_network.getGeometryN( i );
      final Pair<Double, Double> userData = (Pair<Double, Double>)linestring.getUserData();
      final double startWidth = userData.getKey();
      final double endWidth = userData.getValue();
      final Geometry buffer = LineStringBufferBuilder.buffer( linestring, startWidth * m_innerWidthFraction, endWidth * m_innerWidthFraction, startWidth * m_innerWidthFraction, endWidth
          * m_innerWidthFraction );
      polygons.add( buffer );
    }
    Geometry inner = JTSAdapter.jtsFactory.buildGeometry( polygons ).buffer( 0 );
    inner = DouglasPeuckerSimplifier.simplify( inner, m_innerDensifyTolerance / 20 );
    final Polygon innerDensified = (Polygon)Densifier.densify( inner, m_innerDensifyTolerance * 4 );
    innerDensified.apply( minimumDepthFilter );

    final GM_Polygon innerRing = (GM_Polygon)JTSAdapter.wrap( innerDensified, coordinateSystem );
    final GM_PolygonPatch surfacePatch = innerRing.getSurfacePatch();
    final GM_Position[] exteriorRing = surfacePatch.getExteriorRing();
    final GM_Curve innerRingExteriorAsCurve = GeometryFactory.createGM_Curve( exteriorRing, coordinateSystem );
    tinBuilder.addBreakLine( innerRingExteriorAsCurve, false );
    final GM_Position[][] interiorRings = surfacePatch.getInteriorRings();
    if( interiorRings != null )
    {
      for( final GM_Position[] ring : interiorRings )
      {
        final GM_Curve innerRingHoleAsCurve = GeometryFactory.createGM_Curve( ring, coordinateSystem );
        tinBuilder.addBreakLine( innerRingHoleAsCurve, false );
      }
    }
  }
}
