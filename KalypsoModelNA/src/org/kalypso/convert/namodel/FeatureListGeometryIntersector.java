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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.convert.namodel.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.kalypsodeegree_impl.model.sort.SplitSortSpatialIndex;

import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.IntersectionMatrix;
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

  private Envelope m_bbox; // index bounding box

  private final List<Geometry> m_coverBuffer = new ArrayList<Geometry>();

  // better performances if layers with smaller number of (big) polygons are processed first - the reason for sorted
  // map...
  private final SortedMap<Integer, FeatureList> m_sourceLayers = new TreeMap<Integer, FeatureList>();

  private boolean m_initialized = false;

  private long m_numberOfFeatures = 0;

  public FeatureListGeometryIntersector( )
  {
    m_initialized = false;
  }

  public FeatureListGeometryIntersector( final List<FeatureList> sourceFeatureLists )
  {
    for( final FeatureList featureList : sourceFeatureLists )
    {
      if( featureList != null && featureList.size() > 0 )
        m_sourceLayers.put( featureList.size(), featureList );
    }
  }

  public void addFeatureList( final FeatureList featureList )
  {
    if( featureList != null && featureList.size() > 0 )
      m_sourceLayers.put( featureList.size(), featureList );
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
      final Iterator<FeatureList> layers = m_sourceLayers.values().iterator();
      final FeatureList firstLayer = layers.next();
      m_numberOfFeatures = firstLayer.size();
      m_bbox = JTSAdapter.export( firstLayer.getBoundingBox() );
      while( layers.hasNext() )
      {
        final FeatureList layer = layers.next();
        m_numberOfFeatures += layer.size();
        m_bbox.expandToInclude( JTSAdapter.export( layer.getBoundingBox() ) );
      }
      m_index = new SplitSortSpatialIndex( m_bbox );
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
    final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString("org.kalypso.convert.namodel.FeatureListGeometryIntersector.0"), 1000 ); //$NON-NLS-1$

    progress.subTask( Messages.getString("org.kalypso.convert.namodel.FeatureListGeometryIntersector.1") ); //$NON-NLS-1$
    final List<MultiPolygon> resultGeometryList = new ArrayList<MultiPolygon>();
    if( !init() )
      return resultGeometryList;
    progress.worked( 100 );

    /* Find out num feature */
    int countFeatures = 0;
    for( final FeatureList list : m_sourceLayers.values() )
      countFeatures += list.size();
    progress.setWorkRemaining( countFeatures );

    int count = 0;
    for( final FeatureList featureList : m_sourceLayers.values() )
    {
      for( final Feature feature : featureList.toFeatures() )
      {
        if( count % 100 == 0 )
          progress.subTask( Messages.getString("org.kalypso.convert.namodel.FeatureListGeometryIntersector.2", count, countFeatures ) ); //$NON-NLS-1$
        count++;

        final List<Geometry> featurePolygons = removeGeometryCollections( JTSAdapter.export( feature.getDefaultGeometryPropertyValue() ) );
        for( final Geometry featurePolygon : featurePolygons )
        {
          final List<Object> intersectionMetaCandidates = m_index.query( featurePolygon.getEnvelopeInternal() );
          final List<Geometry> intersectionCandidates = removeGeometryCollections( intersectionMetaCandidates );
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
                  // featureGeometry is covered (contained, because they are not equal) by candidateGeometry
                  final Geometry difference = candidatePolygon.difference( featurePolygon );
                  removeFromIndex( candidatePolygon );
                  index( difference );
                  index( featurePolygon );
                  featurePolygonNotInterfere = false;
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
                  final Geometry metaIntersection = candidatePolygon.intersection( featurePolygon );
                  final List<Geometry> intersections = removeGeometryCollections( metaIntersection );
                  if( intersections.size() > 0 )
                  {
                    removeFromIndex( candidatePolygon );
                    final Geometry difference = candidatePolygon.difference( featurePolygon );
                    index( difference );
                    for( final Geometry intersection : intersections )
                    {
                      buffer( intersection );
                      index( intersection );
                    }
                    featurePolygonNotInterfere = false;
                  }
                }
              }
              if( featurePolygonNotInterfere )
                index( featurePolygon );
              else if( m_coverBuffer.size() > 0 )
              {
                final Geometry union = new GeometryFactory().createGeometryCollection( m_coverBuffer.toArray( new Geometry[] {} ) ).buffer( 0.0 );
                index( removeGeometryCollections( featurePolygon.difference( union ) ) );
                m_coverBuffer.clear();
              }
            }
          }
          catch( final Exception e )
          {
            // e.printStackTrace();
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

  private void index( final List<Geometry> removeGeometryCollections )
  {
    for( final Geometry geometry : removeGeometryCollections )
      index( geometry );
  }

  private void index( final Geometry geometry )
  {
    if( geometry instanceof GeometryCollection )
    {
      final GeometryCollection gc = (GeometryCollection) geometry;
      for( int x = 0; x < gc.getNumGeometries(); x++ )
        index( gc.getGeometryN( x ) );
    }
    if( geometry instanceof Polygon || geometry instanceof MultiPolygon )
    {
      final Point point = geometry.getInteriorPoint();
      final List<Object> list = m_index.query( geometry.getEnvelopeInternal() );
      for( final Object object : list )
      {
        final Geometry g = (Geometry) object;
        if( g.covers( point ) )
          return;
      }
      m_index.insert( geometry.getEnvelopeInternal(), geometry );
    }
  }

  private void removeFromIndex( final Geometry geometry )
  {
    if( !m_index.remove( geometry.getEnvelopeInternal(), geometry ) )
    {
      final Point point = geometry.getInteriorPoint();
      final List<Object> list = m_index.query( geometry.getEnvelopeInternal() );
      for( final Object object : list )
      {
        final Geometry g = (Geometry) object;
        if( g.covers( point ) )
        {
          m_index.remove( g.getEnvelopeInternal(), g );
          // return;
        }
      }
    }
  }

  private void buffer( final Geometry geometry )
  {
    if( geometry instanceof Polygon || geometry instanceof MultiPolygon )
      m_coverBuffer.add( geometry );
  }

  private final List<Geometry> removeGeometryCollections( final Object object )
  {
    final List<Object> list = new ArrayList<Object>();
    list.add( object );
    return removeGeometryCollections( list );
  }

  private final List<Geometry> removeGeometryCollections( final List<Object> objects )
  {
    final List<Geometry> list = new ArrayList<Geometry>();
    for( final Object geometry : objects )
    {
      if( ((Geometry) geometry).getArea() == 0.0 )
        continue;
      if( geometry instanceof GeometryCollection )
      {
        final GeometryCollection gc = (GeometryCollection) geometry;
        for( int i = 0; i < gc.getNumGeometries(); i++ )
          list.addAll( removeGeometryCollections( gc.getGeometryN( i ) ) );
      }
      else if( geometry instanceof Polygon )
      {
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
              list.add( geometryN );
          }
        }
        else
          list.add( (Geometry) geometry );
      }
    }
    return list;
  }

}
