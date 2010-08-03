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
package org.kalypso.convert.namodel;

import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.kalypsodeegree_impl.model.sort.SplitSortSpatialIndex;

import com.vividsolutions.jts.algorithm.Angle;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.IntersectionMatrix;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.MultiLineString;
import com.vividsolutions.jts.geom.MultiPoint;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Utility class for intersecting a number of feature geometry layers
 * 
 * @author Dejan Antanaskovic
 */
public class FeatureListGeometryIntersector
{
  private SplitSortSpatialIndex m_index;

  private SplitSortSpatialIndex m_geometryChecked;

  private Envelope m_bbox = null; // index bounding box

  private final List<Geometry> m_coverBuffer = new ArrayList<Geometry>();

  // better performances if layers with smaller number of (big) polygons are processed first - the reason for sorted
  // map...
  private final HashMap<Integer, List<Feature>> m_sourceLayers = new HashMap<Integer, List<Feature>>();

  private boolean m_initialized = false;

  private String m_strCrs = null;

  private List<Geometry> m_intersectionCandidates = null;

  public FeatureListGeometryIntersector( )
  {
    m_initialized = false;
  }

  public FeatureListGeometryIntersector( final List<List<Feature>> sourceFeatureLists )
  {
    for( final List<Feature> featureList : sourceFeatureLists )
    {
      if( featureList != null && featureList.size() > 0 )
        m_sourceLayers.put( m_sourceLayers.size(), featureList );
    }
  }

  public void addFeatureList( final List<Feature> list )
  {
    if( list != null && list.size() > 0 )
      m_sourceLayers.put( m_sourceLayers.size(), list );
  }

  public void clearFeatureList( )
  {
    m_sourceLayers.clear();
    m_initialized = false;
  }

  private boolean init( )
  {
    if( !m_sourceLayers.isEmpty() && !m_initialized )
    {
      for( final List<Feature> list : m_sourceLayers.values() )
      {
        Envelope boundingBox = null;
        if( list instanceof FeatureList )
        {
          boundingBox = JTSAdapter.export( ((FeatureList) list).getBoundingBox() );
        }
        else
        {
          for( final Feature feature : list )
          {
            final GM_Object value = feature.getDefaultGeometryPropertyValue();
            if( value != null )
            {
              final GM_Envelope envelope = value.getEnvelope();
              if( boundingBox == null )
                boundingBox = JTSAdapter.export( envelope );
              else
                boundingBox.expandToInclude( JTSAdapter.export( envelope ) );
            }
          }
        }
        if( boundingBox == null )
          return false;
        if( m_bbox == null )
          m_bbox = boundingBox;
        else
          m_bbox.expandToInclude( boundingBox );
      }
      m_index = new SplitSortSpatialIndex( m_bbox );
      m_geometryChecked = new SplitSortSpatialIndex( m_bbox );

      m_initialized = true;
      return true;
    }
    else
      return false;
  }

  /**
   * Intersects features given as a list of feature lists, produces the result into the resultList<br>
   */
  public List<MultiPolygon> intersect( final IProgressMonitor monitor ) throws GM_Exception, CoreException
  {
    final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString( "org.kalypso.convert.namodel.FeatureListGeometryIntersector.0" ), 1000 ); //$NON-NLS-1$

    progress.subTask( Messages.getString( "org.kalypso.convert.namodel.FeatureListGeometryIntersector.1" ) ); //$NON-NLS-1$
    final List<MultiPolygon> resultGeometryList = new ArrayList<MultiPolygon>();
    if( !init() )
      return resultGeometryList;
    progress.worked( 100 );

    /* Find out num feature */
    int countFeatures = 0;
    for( final List<Feature> list : m_sourceLayers.values() )
      countFeatures += list.size();
    progress.setWorkRemaining( countFeatures );

    int count = 0;
    for( final List<Feature> featureList : m_sourceLayers.values() )
    {
      for( final Feature feature : featureList )
      {
        if( count % 100 == 0 )
          progress.subTask( Messages.getString( "org.kalypso.convert.namodel.FeatureListGeometryIntersector.2", count, countFeatures ) ); //$NON-NLS-1$
        count++;

        GM_Object gmObj = feature.getDefaultGeometryPropertyValue();
        final List<Geometry> featurePolygons = new ArrayList<Geometry>();
        try
        {
          Geometry export = null;
          try
          {
            export = JTSAdapter.export( gmObj );
            featurePolygons.addAll( removeGeometryCollections( normalizeGeometry( export ) ) );
          }
          catch( Exception e )
          {
            // ...
          }
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }
        for( final Geometry featurePolygon : featurePolygons )
        {
          final List<Object> checkedCandidates = m_geometryChecked.query( featurePolygon.getEnvelopeInternal() );
          boolean l_polygonAlreadyChecked = false;

          for( Iterator iterator = checkedCandidates.iterator(); iterator.hasNext(); )
          {
            Geometry geo = (Geometry) iterator.next();

            if( featurePolygon.equalsExact( geo, 0.01 ) )
            {
              l_polygonAlreadyChecked = true;
              break;
            }

          }

          if( l_polygonAlreadyChecked )
          {
            break;
          }
          else
          {
            m_geometryChecked.insert( featurePolygon.getEnvelopeInternal(), featurePolygon );
          }

          final List<Object> intersectionMetaCandidates = m_index.query( featurePolygon.getEnvelopeInternal() );
          final List<Geometry> intersectionCandidates = normalizeGeometry( removeGeometryCollections( intersectionMetaCandidates ) );
          m_intersectionCandidates = new ArrayList<Geometry>();
          m_coverBuffer.clear();
          try
          {
            if( intersectionCandidates.size() == 0 )
              index( featurePolygon );
            else
            {
              boolean featurePolygonNotInterfere = true;

              for( final Geometry candidatePolygon : intersectionCandidates )
              {
                if( featurePolygon.equalsExact( candidatePolygon, 0.0 ) )
                  break;
                final IntersectionMatrix matrix = featurePolygon.relate( candidatePolygon );
                final int dimF = featurePolygon.getDimension();
                final int dimC = candidatePolygon.getDimension();
                if( matrix.isDisjoint() || matrix.isTouches( dimF, dimC ) )
                  continue;
                else if( matrix.isWithin() || matrix.isCoveredBy() )
                {
                  Geometry difference = null;
                  // featureGeometry is covered (contained, because they are not equal) by candidateGeometry
                  try
                  {
                    difference = normalizeGeometry( normalizeGeometry( candidatePolygon ).difference( featurePolygon ) );
                  }
                  catch( Exception e )
                  {
                    e.printStackTrace();
                  }
                  if( difference != null && !isEmptyPolygone( difference ) )
                  {
                    removeFromIndex( candidatePolygon );
                    index( difference );
                    featurePolygonNotInterfere = false;
                  }
                  index( featurePolygon );
                  break;
                }
                else if( matrix.isContains() || matrix.isCovers() )
                {
                  // featureGeometry covers (contains, because they are not equal) candidateGeometry
                  buffer( candidatePolygon );
                  featurePolygonNotInterfere = false;
                  continue;
                }
                else if( matrix.isOverlaps( dimF, dimC ) )
                {
                  // two polygons are (really) intersected, i.e. they have common intersection polygon
                  Geometry metaIntersection = candidatePolygon.intersection( featurePolygon );
                  List<Geometry> intersections = removeGeometryCollections( normalizeGeometry( metaIntersection ) );
                  if( intersections.size() > 0 )
                  {

                    try
                    {
                      Geometry lGeo = ((new GeometryFactory()).createGeometryCollection( intersections.toArray( new Geometry[intersections.size()] ) )).buffer( 0.0 );
                      intersections.clear();
                      intersections.add( normalizeGeometry( lGeo ) );
                    }
                    catch( Exception e )
                    {
                      e.printStackTrace();
                      // TODO: handle exception
                    }

                    if( intersections.size() > 0 )
                    {
                      removeFromIndex( candidatePolygon );
                      Geometry difference = null;
                      // featureGeometry is covered (contained, because they are not equal) by candidateGeometry
                      try
                      {
                        difference = normalizeGeometry( candidatePolygon.difference( featurePolygon ) );
                      }
                      catch( Exception e )
                      {
                        e.printStackTrace();
                      }
                      index( difference );

                      for( final Geometry intersection : intersections )
                      {
                        // buffer( intersection );
                        index( intersection );
                      }
                      try
                      {
                        difference = normalizeGeometry( featurePolygon.difference( candidatePolygon ) );
                      }
                      catch( Exception e )
                      {
                        e.printStackTrace();
                      }
                      index( difference );
                      featurePolygonNotInterfere = false;
                    }
                  }
                }
              }
              if( featurePolygonNotInterfere )
                index( featurePolygon );
              else if( m_coverBuffer.size() > 0 )
              {
                try
                {
                  Geometry union = (new GeometryFactory()).createGeometryCollection( m_coverBuffer.toArray( new Geometry[m_coverBuffer.size()] ) ).union();

                  List<Geometry> unionList = removeGeometryCollections( union );
                  for( Geometry geoUnion : unionList )
                  {

                    Geometry difference = normalizeGeometry( featurePolygon.difference( normalizeGeometry( geoUnion ) ) );
                    index( difference );
                  }
                }
                catch( Exception e )
                {
                  Geometry cutGeo = getUnrolledPolygon( featurePolygon );

                  List<Geometry> listHoles = getUnrolledHolesFromPolygon( featurePolygon );
                  if( listHoles.size() != 0 )
                  {
                    for( Geometry dirko : listHoles )
                    {
                      index( dirko );
                      buffer( dirko );
                    }
                  }

                  boolean brokenPolygonAdded = false;
                  for( Geometry geo : m_coverBuffer )
                  {
// Geometry cutGeo = null;
// featureGeometry is covered (contained, because they are not equal) by candidateGeometry
                    try
                    {
                      cutGeo = (cutGeo.difference( geo )).buffer( 0.0 );
                      cutGeo = normalizeGeometry( cutGeo );
                    }
                    catch( Exception e3 )
                    {
                      brokenPolygonAdded = true;
                    }
                  }
                  if( brokenPolygonAdded )
                    System.out.println( " --------------------------------------------------------------------------------- BROKEN ADDED ************************ " + cutGeo );
                  index( /* removeGeometryCollections( */cutGeo );
                }
                m_coverBuffer.clear();
                if( m_intersectionCandidates.size() > 0 )
                  synchronized( intersectionCandidates )
                  {
                    intersectionCandidates.addAll( m_intersectionCandidates );
                  }
              }
            }
          }
          catch( final Exception e )
          {
            System.out.println( "Big exception:" + feature );
            e.printStackTrace();
          }

          ProgressUtilities.worked( progress, 1 );
        }
      }
    }
    final List<Object> allElements = m_index.query( m_bbox );
    for( final Object element : allElements )
    {
      if( element instanceof MultiPolygon )
        resultGeometryList.add( (MultiPolygon) element );
      else if( element instanceof Polygon )
      {
        final MultiPolygon multiPolygon = new MultiPolygon( new Polygon[] { (Polygon) element }, new GeometryFactory() );
        resultGeometryList.add( multiPolygon );
      }
    }
    return resultGeometryList;
  }

  private boolean isEmptyPolygone( final Geometry difference )
  {
    if( difference instanceof GeometryCollection || difference instanceof MultiPolygon )
    {
      final GeometryCollection gc = (GeometryCollection) difference;
      for( int x = 0; x < gc.getNumGeometries(); x++ )
      {
        if( gc.getGeometryN( x ).getCoordinates().length > 0 )
          return false;
      }
    }
    else
    {
      return difference.getCoordinates().length == 0;
    }
    return false;
  }

  public List<Geometry> normalizeGeometry( List<Geometry> removeGeometryCollections )
  {
    List<Geometry> lListRes = new ArrayList<Geometry>();
    for( final Geometry geometry : removeGeometryCollections )
    {
      Geometry normalizedGeometry = normalizeGeometry( geometry );
      if( normalizedGeometry != null )
        lListRes.add( normalizedGeometry );
    }
    return lListRes;
  }

  private boolean index( final Geometry geometry )
  {
    if( geometry instanceof GeometryCollection || geometry instanceof MultiPolygon )
    {
      final GeometryCollection gc = (GeometryCollection) geometry;
      boolean indexed = false;
      for( int x = 0; x < gc.getNumGeometries(); x++ )
      {
        if( gc.getGeometryN( x ).getCoordinates().length > 0 )
        {
          index( gc.getGeometryN( x ) );
          indexed = true;
        }
      }
      return indexed;
    }
    else if( geometry instanceof Polygon )
    {
      if( geometry.getCoordinates().length == 0 || isInvalidArea( geometry ) )
        return false;

      Point point = getJustInteriorPointFixed( geometry );
      final List<Object> list = m_index.query( geometry.getEnvelopeInternal() );
      for( final Object object : list )
      {
        final Geometry g = (Geometry) object;
        {
          if( g.covers( point ) )
            return false;
        }
      }
      m_index.insert( geometry.getEnvelopeInternal(), geometry );
      return true;
    }
    return false;
  }

  private void removeFromIndex( final Geometry geometry )
  {
    if( !m_index.remove( geometry.getEnvelopeInternal(), geometry ) )
    {
      Point point = getJustInteriorPointFixed( geometry );
// Point point = geometry.getInteriorPoint();
      final List<Object> list = m_index.query( geometry.getEnvelopeInternal() );
      for( final Object object : list )
      {
        final Geometry g = (Geometry) object;
        if( g.covers( point ) )
        {
          m_index.remove( g.getEnvelopeInternal(), g );
        }
      }
    }
  }

  private void buffer( final Geometry geometry )
  {
    if( geometry instanceof Polygon || geometry instanceof MultiPolygon )
      if( geometry.getCoordinates().length > 0 )
      {
        m_coverBuffer.add( geometry );
      }
  }

  private void appendCandidate( final List<Coordinate> coordsOrig )
  {
    if( coordsOrig == null || coordsOrig.size() < 4 )
      return;

    List<Coordinate> coords = new ArrayList<Coordinate>( coordsOrig );

    if( isSnappy( coords.get( coords.size() - 1 ), coords.get( 0 ) ) )
    {
      coords.remove( coords.size() - 1 );
    }
    coords.add( coords.get( 0 ) );

    LinearRing exteriorRing = new GeometryFactory().createLinearRing( coords.toArray( new Coordinate[coords.size()] ) );

    appendCandidate( new GeometryFactory().createPolygon( exteriorRing, null ) );
  }

  private void appendCandidate( Geometry geo )
  {
// System.err.println( "---- Appending candidate: " + geo );
    m_intersectionCandidates.add( geo );
  }

  private final List<Geometry> removeGeometryCollections( final Object object )
  {
    final List<Object> list = new ArrayList<Object>();
    if( object != null )
      list.add( object );
    return removeGeometryCollections( list );
  }

  final static double snapDistance = (0.01 * 0.1);

  private final boolean isSnappy( final Coordinate first, final Coordinate second )
  {
    double x = Math.abs( first.x - second.x );
    double y = Math.abs( first.y - second.y );

    return (x * x + y * y) < snapDistance;
  }

  private List<Coordinate> normalizeRing( final Coordinate[] coordinatesList, final Geometry ringEnv )
  {
    try
    {
      Coordinate previous = new Coordinate( Double.NaN, Double.NaN );
      Coordinate prePrev = new Coordinate( Double.NaN, Double.NaN );

      final SplitSortSpatialIndex index = new SplitSortSpatialIndex( ringEnv.getEnvelopeInternal() );
      final SplitSortSpatialIndex indexEdges = new SplitSortSpatialIndex( ringEnv.getEnvelopeInternal() );
      List<Coordinate> coordListNew = new ArrayList<Coordinate>();
      for( int i = 0; i < coordinatesList.length - 1; i++ )
      {
        Coordinate coord = coordinatesList[i];
        if( isSnappy( coord, prePrev ) == true )
        {
          Coordinate to_remove = coordListNew.get( coordListNew.size() - 1 );
          index.remove( new Envelope( to_remove ), to_remove );
          coordListNew.remove( to_remove );

          previous = prePrev;
          if( coordListNew.size() > 1 )
            prePrev = coordListNew.get( coordListNew.size() - 2 );
          else
            prePrev = new Coordinate( Double.NaN, Double.NaN );

          continue;
        }

        if( isSnappy( coord, previous ) == false )
        {
          List<Object> list = index.query( createSnapEnvelope( coord ) );

          if( list != null && list.size() > 0 )
          {
            List<Coordinate> lListToRemove = new ArrayList<Coordinate>();

            boolean boolFoundToRemove = false;
            for( int j = 0; j < coordListNew.size(); ++j )
            {
              if( isSnappy( coordListNew.get( j ), (Coordinate) list.get( 0 ) ) )
              {
                boolFoundToRemove = !boolFoundToRemove;
              }

              if( boolFoundToRemove )
              {
                lListToRemove.add( coordListNew.get( j ) );
              }
            }
            coordListNew.removeAll( lListToRemove );
            appendCandidate( lListToRemove );
            list.clear();
            continue;

          }
          else
          {
            List<Object> listEdges = indexEdges.query( createSnapEnvelope( coord ) );
            if( listEdges != null && listEdges.size() > 0 )
            {
              List<Coordinate> lListToRemove = new ArrayList<Coordinate>();
              lListToRemove.clear();
              boolean boolFoundToRemove = false;
              for( int j = 0; j < listEdges.size(); ++j )
              {
                Coordinate edgeCoordFirst = ((Coordinate[]) (listEdges.get( j )))[0];
                Coordinate edgeCoordSecond = ((Coordinate[]) (listEdges.get( j )))[1];
                if( isOnLine( GeometryFactory.createPointFromInternalCoord( edgeCoordFirst, ringEnv ), GeometryFactory.createPointFromInternalCoord( edgeCoordSecond, ringEnv ), GeometryFactory.createPointFromInternalCoord( coord, ringEnv ) ) )
                {
                  for( int k = 0; k < coordListNew.size(); ++k )
                  {
                    if( boolFoundToRemove )
                    {
                      lListToRemove.add( coordListNew.get( k ) );
                    }
// if( isSnappy( coordListNew.get( k ), edgeCoordFirst ) || isSnappy( coordListNew.get( k ), edgeCoordSecond ) )
                    if( coordListNew.get( k ).equals3D( edgeCoordFirst ) || coordListNew.get( k ).equals3D( edgeCoordSecond ) )
                    {
                      boolFoundToRemove = !boolFoundToRemove;
                    }
                  }
                }
              }
              if( lListToRemove.size() > 0 )
              {
                coordListNew.removeAll( lListToRemove );
                appendCandidate( lListToRemove );
              }
              listEdges.clear();
            }
            if( coordListNew.size() > 0 )
            {
              Coordinate actualPrev = coordListNew.get( coordListNew.size() - 1 );
              indexEdges.insert( new Envelope( actualPrev, coord ), new Coordinate[] { actualPrev, coord } );
            }
            coordListNew.add( coord );
            index.insert( new Envelope( coord ), coord );

            prePrev = previous;
            previous = coord;
          }

        }
        else
        {
          // Simple snap: ignore the point and do not add it to the newly created polygon
          continue;
        }
      }

      // check the last point
      Coordinate last = coordinatesList[coordinatesList.length - 1];
      if( isSnappy( previous, last ) )
      {
        coordListNew.remove( coordListNew.size() - 1 );
      }

      // Definitely add last point == first point
      coordListNew.add( last );
      return coordListNew;
    }
    catch( Throwable e )
    {
      e.printStackTrace();
      // TODO: handle exception
    }
    return null;
  }

  private Envelope createSnapEnvelope( final Coordinate coord )
  {
// return new Envelope( new Coordinate( coord.x - snapDistance / 2, coord.y - snapDistance / 2 ), new Coordinate(
// coord.x + snapDistance / 2, coord.y + snapDistance / 2 ) );
    return new Envelope( new Coordinate( coord.x - snapDistance, coord.y - snapDistance ), new Coordinate( coord.x + snapDistance, coord.y + snapDistance ) );
  }

  private Geometry normalizePolygon( final Polygon poly )
  {
    List<Coordinate> exteriorListNew = normalizeRing( poly.getExteriorRing().getCoordinates(), poly.getEnvelope() );

    // not even a triangle !
    if( exteriorListNew.size() < 4 )
    {
      // remove the polygon
      return null;
    }

    LinearRing exteriorRing = new GeometryFactory().createLinearRing( exteriorListNew.toArray( new Coordinate[exteriorListNew.size()] ) );

    // unroll holes
    List<LinearRing> holes = new ArrayList<LinearRing>();
    for( int i = 0; i < poly.getNumInteriorRing(); i++ )
    {
      List<Coordinate> interiorListNew = normalizeRing( poly.getInteriorRingN( i ).getCoordinates(), poly.getInteriorRingN( i ).getEnvelope() );
      // not even a triangle !
      if( interiorListNew.size() < 4 )
      {
        // remove the hole
        continue;
      }

      LinearRing interiorRing = new GeometryFactory().createLinearRing( interiorListNew.toArray( new Coordinate[interiorListNew.size()] ) );

      // test if some other hole touches this one. otherwise difference method on this geometry will nto work
      for( LinearRing ring : holes )
      {
        if( interiorRing.touches( ring ) || interiorRing.intersects( ring ) )
        {
          interiorRing = normalizeHole( interiorRing, ring );
        }
      }

      if( interiorRing != null )
        holes.add( interiorRing );
    }

    return new GeometryFactory().createPolygon( exteriorRing, holes.toArray( new LinearRing[holes.size()] ) );
  }

  private LinearRing normalizeHole( LinearRing holeToTest, LineString oldHole )
  {
    final SplitSortSpatialIndex index = new SplitSortSpatialIndex( oldHole.getEnvelopeInternal() );
    List<Coordinate> coordListNew = new ArrayList<Coordinate>();

    // populate index
    for( int i = 0; i < oldHole.getCoordinates().length - 1; i++ )
      index.insert( new Envelope( oldHole.getCoordinateN( i ) ), oldHole.getCoordinateN( i ) );

    for( int i = 0; i < holeToTest.getCoordinates().length - 1; i++ )
    {
      Coordinate point = holeToTest.getCoordinateN( i );
      List<Object> list = index.query( createSnapEnvelope( point ) );

      if( list != null && list.size() > 0 )
      {
        for( Object obj : list )
        {
          Coordinate coord = (Coordinate) obj;

          if( isSnappy( point, coord ) )
          {
            Point a = new GeometryFactory().createPoint( point );
            Point b = holeToTest.getCentroid();

            double deltaX = b.getX() - a.getX();
            double deltaY = b.getY() - a.getY();
            double gip = a.distance( b );

            double fx = Math.signum( deltaX ) * 3.0 * snapDistance * (deltaX * deltaX) / gip;
            double fy = Math.signum( deltaX ) * 3.0 * snapDistance * (deltaY * deltaY) / gip;

            point = new Coordinate( a.getX() + fx, a.getY() + fy );
          }
        }

        coordListNew.add( point );
      }

    }

    LinearRing resultRing = null;
    if( coordListNew.size() > 3 )
      resultRing = new GeometryFactory().createLinearRing( coordListNew.toArray( new Coordinate[coordListNew.size()] ) );

    return resultRing;
  }

  private void debugPrint( final Polygon poly )
  {
    FileWriter writer;
    List<Coordinate> exteriorListNew = Arrays.asList( poly.getExteriorRing().getCoordinates() );
    try
    {
      writer = new FileWriter( "e:\\db_temp\\poly.txt", true );
      writer.write( "----------------------- \r\n" );
      writer.write( "POLYGON: " + poly.getSRID() + "\r\n" );

      for( Coordinate c : exteriorListNew )
      {
        String a = "" + c.x + "\t" + c.y + "\r\n";
        a.replace( '.', ',' );
        writer.write( a );
      }
      writer.write( "----------------------- \r\n" );
      writer.close();
    }
    catch( IOException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  private Geometry normalizeGeometry( final Geometry geometry )
  {
    if( geometry instanceof Point || geometry instanceof MultiPoint || geometry instanceof LineString || geometry instanceof MultiLineString )
    {
      return null;// geometry;
    }

    if( geometry instanceof Polygon )
    {
      if( geometry.getCoordinates().length == 0 )
        return null;

      return normalizePolygon( (Polygon) geometry );
    }

    if( geometry instanceof MultiPolygon || geometry instanceof GeometryCollection )
    {
      final GeometryCollection collection = (GeometryCollection) geometry;
      List<Geometry> newCol = new ArrayList<Geometry>();
      for( int x = 0; x < collection.getNumGeometries(); x++ )
      {

        Geometry normalizedGeometry = normalizeGeometry( collection.getGeometryN( x ) );
        if( normalizedGeometry != null )
          newCol.add( normalizedGeometry );
      }

      if( newCol.size() == 0 )
        return null;

      GeometryCollection geometryCollection = new GeometryFactory().createGeometryCollection( GeometryFactory.toGeometryArray( newCol ) );
      return geometryCollection;
    }

    return geometry;
  }

  private Polygon getUnrolledPolygon( final Geometry geometry )
  {

    if( geometry instanceof Polygon )
    {
      Polygon poly = (Polygon) geometry;

      LinearRing exteriorRing = new GeometryFactory().createLinearRing( poly.getExteriorRing().getCoordinates() );
      Polygon firstPoly = new GeometryFactory().createPolygon( exteriorRing, null );

      return firstPoly;
    }

    return null;
  }

  private List<Geometry> getUnrolledHolesFromPolygon( final Geometry geometry )
  {
    List<Geometry> listResult = new ArrayList<Geometry>();

    if( geometry instanceof Polygon )
    {
      Polygon poly = (Polygon) geometry;

      // unroll holes
      for( int i = 0; i < poly.getNumInteriorRing(); i++ )
      {
        LinearRing interiorRing = new GeometryFactory().createLinearRing( poly.getInteriorRingN( i ).getCoordinates() );
        Polygon newPoly = new GeometryFactory().createPolygon( interiorRing, null );
        listResult.add( newPoly );
      }

    }

    return listResult;
  }

  private List<Geometry> unrollHoles( final Geometry geometry )
  {
    List<Geometry> listResult = new ArrayList<Geometry>();

    if( geometry instanceof Point || geometry instanceof MultiPoint || geometry instanceof LineString || geometry instanceof MultiLineString )
    {
      listResult.add( geometry );
    }
    else if( geometry instanceof Polygon )
    {
      listResult.add( getUnrolledPolygon( geometry ) );
      listResult.addAll( getUnrolledHolesFromPolygon( geometry ) );
    }
    else if( geometry instanceof MultiPolygon || geometry instanceof GeometryCollection )
    {
      final GeometryCollection collection = (GeometryCollection) geometry;
      for( int x = 0; x < collection.getNumGeometries(); x++ )
      {
        listResult.addAll( unrollHoles( collection.getGeometryN( x ) ) );
      }
    }

    return listResult;
  }

  private final List<Geometry> removeGeometryCollections( final List<Object> objects )
  {
    final List<Geometry> list = new ArrayList<Geometry>();
    for( final Object geometry : objects )
    {
      if( ((Geometry) geometry).getArea() == 0.0 )
      {
        continue;
      }
      if( geometry instanceof GeometryCollection )
      {
        final GeometryCollection gc = (GeometryCollection) geometry;
        for( int i = 0; i < gc.getNumGeometries(); i++ )
        {
          list.addAll( removeGeometryCollections( gc.getGeometryN( i ) ) );
        }
      }
      else if( geometry instanceof Polygon )
      {
        if( isInvalidArea( (Geometry) geometry ) )
        {
          continue;
        }
        list.add( (Geometry) geometry );
      }
      else if( geometry instanceof MultiPolygon )
      {
        if( geometry instanceof GeometryCollection )
        {
          final GeometryCollection collection = (GeometryCollection) geometry;
          for( int x = 0; x < collection.getNumGeometries(); x++ )
          {
            final Geometry geometryN = collection.getGeometryN( x );
            if( geometryN instanceof Polygon || geometryN instanceof MultiPolygon )
            {
              list.add( geometryN );
            }
          }
        }
        else
          list.add( (Geometry) geometry );
      }
    }
    return list;
  }

  private boolean isInvalidArea( final Geometry geometry )
  {
    return (geometry.getArea() / geometry.getEnvelope().getArea()) < 0.001;
  }

  public Point getJustInteriorPointFixed( final Geometry geometry )
  {
    if( geometry instanceof Point )
    {
      return (Point) geometry;
    }

    if( geometry instanceof MultiPoint || geometry instanceof LineString || geometry instanceof MultiLineString )
    {
      //
      return null;
    }

    if( geometry instanceof Polygon )
    {
      return getJustInteriorPointFixed( (Polygon) geometry );
    }

    if( geometry instanceof MultiPolygon || geometry instanceof GeometryCollection )
    {
// final GeometryCollection collection = (GeometryCollection) geometry;
// List<Geometry> newCol = new ArrayList<Geometry>();
// for( int x = 0; x < collection.getNumGeometries(); x++ )
// {
// Geometry normalizedGeometry = normalizeGeometry( collection.getGeometryN( x ) );
// if( normalizedGeometry != null )
// newCol.add( normalizedGeometry );
// }
//
// if( newCol.size() == 0 )
// return null;

      return getJustInteriorPointFixed( geometry.getGeometryN( 0 ) );
    }

    return GeometryFactory.createPointFromInternalCoord( geometry.getCoordinates()[0], geometry );
  }

  public Point getJustInteriorPointFixed( final Polygon poly )
  {
    Point point = null;

    point = poly.getCentroid();
    if( isPointInsidePolygon( poly, point ) == true )
      return point;

    // ###
    // second method, shifted centroid
    for( int i = 0; i < poly.getExteriorRing().getNumPoints() - 1; i++ )
    {
      Point b = poly.getExteriorRing().getPointN( i );

      double deltaX = b.getX() - point.getX();
      double deltaY = b.getY() - point.getY();
      double gip = point.distance( b );

      double fx = Math.signum( deltaX ) * 4.0 / 5.0 * (deltaX * deltaX) / gip;
      double fy = Math.signum( deltaY ) * 4.0 / 5.0 * (deltaY * deltaY) / gip;

      Point np = new GeometryFactory().createPoint( new Coordinate( point.getX() + fx, point.getY() + fy ) );

      if( isPointInsidePolygon( poly, np ) == true )
        return np;
    }

    // ###
    // third method, walking triangle
    for( int index = 0; index < poly.getExteriorRing().getNumPoints() - 2; index++ )
    {
      Point a = poly.getExteriorRing().getPointN( index );
      Point b = poly.getExteriorRing().getPointN( index + 1 );
      Point c = poly.getExteriorRing().getPointN( poly.getExteriorRing().getNumPoints() - 2 - index );

      double centroidX = (a.getX() + b.getX() + c.getX()) / 3;
      double centroidY = (a.getY() + b.getY() + c.getY()) / 3;

// System.out.println("Triangle:");
// System.out.println("" + a.getX() + "\t" + a.getY());
// System.out.println("" + b.getX() + "\t" + b.getY());
// System.out.println("" + c.getX() + "\t" + c.getY());
// System.out.println("" + centroidX + "\t" + centroidY);

      Point np = new GeometryFactory().createPoint( new Coordinate( centroidX, centroidY ) );

      if( isPointInsidePolygon( poly, np ) == true )
        return np;
    }

    // ###
    // last method! angle analysis of each points area
    for( int index = 1; index < poly.getExteriorRing().getNumPoints() - 1; index++ )
    {
      Point pa = poly.getExteriorRing().getPointN( index - 1 );
      Point pb = poly.getExteriorRing().getPointN( index );
      Point pc = poly.getExteriorRing().getPointN( index + 1 );

      // lower bound
      double distA = Math.max( snapDistance * 4, pa.distance( pb ) / 2 );
      double distC = Math.max( snapDistance * 4, pc.distance( pb ) / 2 );

      // upper bound
      distA = Math.min( snapDistance * 1000, distA );
      distC = Math.min( snapDistance * 1000, distC );

      double dist = Math.min( distA, distC );

      Coordinate ca = new Coordinate( pa.getX(), pa.getY() );
      Coordinate cb = new Coordinate( pb.getX(), pb.getY() );
      Coordinate cc = new Coordinate( pc.getX(), pc.getY() );

      double angle = Angle.angleBetween( ca, cb, cc );
      if( angle < Math.PI / 180 )
        continue;
      angle /= 2.1;

      for( double rot = 0; rot < 2 * Math.PI; rot += angle )
      {
        Coordinate runner = new Coordinate( pb.getX() + Math.cos( rot ) * dist, pb.getY() + Math.sin( rot ) * dist );
        Point np = new GeometryFactory().createPoint( runner );

        if( isPointInsidePolygon( poly, np ) == true )
          return np;
      }

    }

    System.out.println( "Generated bad interior point: " + poly );
    return GeometryFactory.createPointFromInternalCoord( poly.getCoordinates()[0], poly );
  }

  public static final boolean isPointInsidePolygon( final LineString pos, final Point point )
  {
    boolean isInside = false;

    Point posJ = pos.getPointN( 0 );
    Point posI = pos.getPointN( 0 );
    for( int i = 1; i < pos.getNumPoints(); i++ )
    {
      posI = posJ;
      posJ = pos.getPointN( i );

      if( isOnLine( posI, posJ, point ) )
        return false;

      /*
       * if (y[i] < pointY && y[j] >= pointY || y[j] < pointY && y[i] >= pointY) { nx = x - pos[0].x; if (x[i] + (pointY
       * - y[i]) / (y[j] - y[i]) * (x[j] - x[i]) < pointX) { isInside = !isInside; } }
       */
      if( posI.getY() < point.getY() && posJ.getY() >= point.getY() || posJ.getY() < point.getY() && posI.getY() >= point.getY() )
      {
        if( (posI.getX() - point.getX()) + (point.getY() - posI.getY()) / (posJ.getY() - posI.getY()) * (posJ.getX() - posI.getX()) < 0 )
        {
          isInside = !isInside;
        }

      }
    }

    return isInside;
  }

  private static final boolean isOnLine( final Point endPoint1, final Point endPoint2, final Point checkPoint )
  {
    return (checkPoint.getY() - endPoint1.getY()) / (endPoint2.getY() - endPoint1.getY()) == (checkPoint.getX() - endPoint1.getX()) / (endPoint2.getX() - endPoint1.getX());
  }

  public static final boolean AboutEqual( final double x, final double y )
  {
    return Math.abs( x - y ) <= GM_Position.MUTE;
  }

  public static final boolean isPointInsidePolygon( final Geometry geo, final Point point )
  {
    if( geo instanceof Polygon )
    {
      Polygon poly = (Polygon) geo;
      final LineString exteriorRing = poly.getExteriorRing();

      if( isPointInsidePolygon( exteriorRing, point ) == false )
        return false;

      if( poly.getNumInteriorRing() > 0 )
        for( int i = 0; i < poly.getNumInteriorRing(); i++ )
          if( isPointInsidePolygon( poly.getInteriorRingN( i ), point ) == true )
            return false;

      return true;
    }

    return false;
  }

}
