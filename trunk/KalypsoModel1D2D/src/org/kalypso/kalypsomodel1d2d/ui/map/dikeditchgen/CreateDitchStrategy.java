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

import org.apache.commons.lang3.tuple.Pair;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.jts.JTSUtilities;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.elevation.IElevationModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.densify.Densifier;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateFilter;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.noding.SegmentString;
import com.vividsolutions.jts.simplify.DouglasPeuckerSimplifier;

/**
 * @author kurzbach
 */
public class CreateDitchStrategy implements CreateStructuredNetworkStrategy
{

  private final double m_innerWidthFraction;

  final double m_minimumDepth;

  final IElevationModel m_elevationModel;

  // working network features
  private FeatureList m_network;

  private IPropertyType m_geomProperty;

  private IPropertyType m_startWidthProperty;

  private IPropertyType m_endWidthProperty;

  public CreateDitchStrategy( final double innerWidthFraction, final double minimumDepth, final IElevationModel elevationModel )
  {
    m_innerWidthFraction = innerWidthFraction;
    m_minimumDepth = minimumDepth;
    m_elevationModel = elevationModel;
  }

  public void setFeatures( final FeatureList network, IPropertyType geomProperty, IPropertyType startWidthProperty, IPropertyType endWidthProperty )
  {
    m_network = network;
    m_geomProperty = geomProperty;
    m_startWidthProperty = startWidthProperty;
    m_endWidthProperty = endWidthProperty;
  }

  @Override
  public void addBoundary( final TriangulationBuilder tinBuilder ) throws GM_Exception
  {
    final String coordinateSystem = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    final double densifyFactor = m_innerWidthFraction > 0.5 ? 2 / m_innerWidthFraction : m_innerWidthFraction * 4;
    final InterpolateElevationFilter ringFilter = new InterpolateElevationFilter( coordinateSystem, m_elevationModel );
    final GM_Polygon outerRing = getRingPolygon( 1.0, densifyFactor, null, ringFilter );
    tinBuilder.setBoundary( outerRing, false );
  }

  private GM_Polygon getRingPolygon( final double widthFactor, double densifyFactor, final CoordinateFilter networkFilter, final CoordinateFilter ringFilter ) throws GM_Exception
  {
    final int networkSize = m_network.size();
    final Collection<SegmentString> bufferSegStrList = new ArrayList<>( networkSize );
    double minimumWidth = Double.MAX_VALUE;
    for( int i = 0; i < networkSize; i++ )
    {
      final Feature linearFeature = m_network.getResolved( i );
      final LineString[] lines = getLines( linearFeature, densifyFactor, networkFilter );
      final Pair<Double, Double> startEnd = getWidth( linearFeature );
      final double minWidth = Math.min( startEnd.getLeft(), startEnd.getRight() );
      minimumWidth = minWidth < minimumWidth ? minWidth : minimumWidth;
      for( int j = 0; j < lines.length; j++ )
        LineStringBufferBuilder.addRawBufferLines( lines[j], startEnd.getLeft() * widthFactor, startEnd.getRight() * widthFactor, startEnd.getLeft() * widthFactor, startEnd.getRight() * widthFactor, bufferSegStrList );
    }
    final Polygon outerDensified = (Polygon)LineStringBufferBuilder.buffer( bufferSegStrList );
    final Polygon filtered = JTSUtilities.removeCoincidentPoints( outerDensified, minimumWidth * widthFactor / 2 );
    if( ringFilter != null )
      filtered.apply( ringFilter );
    final GM_Polygon outerRing = (GM_Polygon)JTSAdapter.wrap( filtered, KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
    return outerRing;
  }

  private LineString[] getLines( final Feature linearFeature, final double densifyFactor, CoordinateFilter networkFilter )
  {
    try
    {
      // convert to jts, add Z, simplify and densify
      final GM_Object geometry = (GM_Object)linearFeature.getProperty( m_geomProperty );
      final Geometry linestring = JTSAdapter.export( geometry ).getGeometryN( 0 );
      final Pair<Double, Double> startEnd = getWidth( linearFeature );
      final double maxWidth = Math.max( startEnd.getLeft(), startEnd.getRight() );
      final LineString[] results = new LineString[linestring.getNumGeometries()];
      for( int i = 0; i < results.length; i++ )
      {
        final LineString linestringZ = JTSUtilities.interpolateMissingZ( (LineString)linestring.getGeometryN( i ) );
        if( networkFilter != null )
          linestringZ.apply( networkFilter );
        final LineString simplified = (LineString)DouglasPeuckerSimplifier.simplify( linestringZ, maxWidth / 1000 );
        results[i] = (LineString)Densifier.densify( simplified, maxWidth * densifyFactor );
      }
      return results;
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      return new LineString[0];
    }
  }

  private Pair<Double, Double> getWidth( final Feature linearFeature )
  {
    final Double startWidth = (Double)linearFeature.getProperty( m_startWidthProperty );
    final Double endWidth = (Double)linearFeature.getProperty( m_endWidthProperty );
    final Pair<Double, Double> startEnd = Pair.of( startWidth, endWidth );
    return startEnd;
  }

  @Override
  public void addBreaklines( final TriangulationBuilder tinBuilder ) throws GM_Exception
  {
    final String coordinateSystem = getCoordinateSystem();
    final int networkSize = m_network.size();

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

    final double densifyFactor = m_innerWidthFraction > 0.5 ? 4 / m_innerWidthFraction : m_innerWidthFraction * 2;
    for( int i = 0; i < networkSize; i++ )
    {
      final Feature linearFeature = m_network.getResolved( i );
      final LineString[] lines = getLines( linearFeature, densifyFactor, minimumDepthFilter );
      for( int j = 0; j < lines.length; j++ )
      {
        final LineString linestringZ = JTSUtilities.interpolateMissingZ( lines[j] );
        linestringZ.apply( minimumDepthFilter );
        final GM_Curve edgeCurve = (GM_Curve)JTSAdapter.wrap( linestringZ, coordinateSystem );
        tinBuilder.addBreakLine( edgeCurve, false );
      }
    }
    final GM_Polygon inner = getRingPolygon( m_innerWidthFraction, densifyFactor, minimumDepthFilter, null );

    // exterior ring
    final GM_Curve exteriorRingAsCurve = GeometryFactory.createGM_Curve( inner.getSurfacePatch().getExteriorRing(), coordinateSystem );
    tinBuilder.addBreakLine( exteriorRingAsCurve, false );

    // interior rings
    final GM_Position[][] interiorRings = inner.getSurfacePatch().getInteriorRings();
    for( int i = 0; i < interiorRings.length; i++ )
    {
      final GM_Position[] interiorRing = interiorRings[i];
      final GM_Curve innerRingAsCurve = GeometryFactory.createGM_Curve( interiorRing, coordinateSystem );
      tinBuilder.addBreakLine( innerRingAsCurve, false );
    }
  }

  private String getCoordinateSystem( )
  {
    return m_network.getResolved( 0 ).getDefaultGeometryPropertyValue().getCoordinateSystem();
  }
}
