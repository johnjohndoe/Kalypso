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
package org.kalypso.model.hydrology.operation.hydrotope;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.kalypsodeegree_impl.model.sort.SplitSortSpatialIndex;

import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.IntersectionMatrix;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Utility class for intersecting a number of feature geometry layers
 * <p>
 * refactored by Stefan Kurzbach
 * <p>
 * commented out some code for fixing bad geometries
 * 
 * @author Dejan Antanaskovic
 */
public class FeatureListGeometryIntersector
{
  private static final double MIN_AREA = 0.01;

  private final Map<List<Feature>, SplitSortSpatialIndex> m_index = new HashMap<List<Feature>, SplitSortSpatialIndex>();

  private final List<List<Feature>> m_sourceLayers = new ArrayList<List<Feature>>();

  private final List<IStatus> m_log = new ArrayList<IStatus>( 1000 );

  public void addFeatureList( final List<Feature> list )
  {
    if( list != null && list.size() > 0 )
      m_sourceLayers.add( list );
  }

  private void init( )
  {
    for( final List<Feature> list : m_sourceLayers )
    {
      Envelope boundingBox = new Envelope();
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
            boundingBox.expandToInclude( JTSAdapter.export( envelope ) );
          }
        }
      }
      m_index.put( list, new SplitSortSpatialIndex( boundingBox ) );
    }
  }

  /**
   * Intersects features given as a list of feature lists, produces the result into the resultList<br>
   */
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public List<Polygon> intersect( final IProgressMonitor monitor )
  {
    if( m_sourceLayers.isEmpty() )
      return Collections.EMPTY_LIST;

    final int layerCount = m_sourceLayers.size();
    final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString( "org.kalypso.convert.namodel.FeatureListGeometryIntersector.0" ), layerCount * (layerCount + 1) / 2 + 1 ); //$NON-NLS-1$
    progress.subTask( Messages.getString( "org.kalypso.convert.namodel.FeatureListGeometryIntersector.1" ) ); //$NON-NLS-1$
    init();
    progress.worked( 2 );

    final Map<List<Feature>, Integer> sizeMap = new HashMap<List<Feature>, Integer>( m_sourceLayers.size() );
    for( final List<Feature> featureList : m_sourceLayers )
    {
      int polyCount = 0;
      final SplitSortSpatialIndex index = m_index.get( featureList );
      for( final Feature feature : featureList )
      {
        final GM_Object gmObj = feature.getDefaultGeometryPropertyValue();

        Geometry export = null;
        try
        {
          export = JTSAdapter.export( gmObj );
          if( !export.isEmpty() )
          {
            for( int n = 0; n < export.getNumGeometries(); n++ )
            {
              final Geometry geometryN = export.getGeometryN( n );
              geometryN.setUserData( new FeatureIntersection( feature ) );
              index.insert( geometryN.getEnvelopeInternal(), geometryN );
              polyCount++;
            }
          }
        }
        catch( final Exception e )
        {
          m_log.add( StatusUtilities.createStatus( IStatus.WARNING, Messages.getString("FeatureListGeometryIntersector.3") + feature.getName(), e ) ); //$NON-NLS-1$
        }
      }
      sizeMap.put( featureList, polyCount );
    }

    // sort by real number of polygons
    Collections.sort( m_sourceLayers, new Comparator<List<Feature>>()
    {

      @Override
      public int compare( final List<Feature> o1, final List<Feature> o2 )
      {
        return sizeMap.get( o1 ) - sizeMap.get( o2 );
      }
    } );

    final Iterator<List<Feature>> sourceListIterator = m_sourceLayers.iterator();
    final List<Feature> sourceList = sourceListIterator.next();
    final SplitSortSpatialIndex sourceIndex = m_index.get( sourceList );
    List<Object> sourcePolygons = sourceIndex.query( sourceIndex.getBoundingBox() );
    List resultGeometryList = null;
    int layer = 0;
    double totalAreaDiscarded = 0;
    while( sourceListIterator.hasNext() )
    {
      resultGeometryList = new ArrayList();
      final List<Feature> targetList = sourceListIterator.next();
      layer++;
      final int countSourceFeatures = sourcePolygons.size();
      final int countTargetFeatures = sizeMap.get( targetList );
      final SubMonitor childProgress = SubMonitor.convert( progress.newChild( layer + 1 ), countSourceFeatures );
      int count = 0;
      for( final Object thisObject : sourcePolygons )
      {
        if( count % 10 == 0 )
        {
          final String msg = Messages.getString( "org.kalypso.convert.namodel.FeatureListGeometryIntersector.2", count, countSourceFeatures, countTargetFeatures, layer + 1 ); //$NON-NLS-1$
          progress.subTask( msg ); //$NON-NLS-1$
          childProgress.worked( 10 );
        }
        count++;

        final Geometry featurePolygon = (Geometry) thisObject;

        final Object userData1 = featurePolygon.getUserData();
        final int dim1 = featurePolygon.getDimension();

        final SplitSortSpatialIndex targetIndex = m_index.get( targetList );
        final List<Object> candidates = targetIndex.query( featurePolygon.getEnvelopeInternal() );

        for( final Object candidateObject : candidates )
        {
          final Geometry candidatePolygon = (Geometry) candidateObject;

          final Object userData2 = candidatePolygon.getUserData();
          final int dim2 = candidatePolygon.getDimension();

          final FeatureIntersection userData = new FeatureIntersection( (FeatureIntersection) userData1, (FeatureIntersection) userData2 );

          final IntersectionMatrix relate = featurePolygon.relate( candidatePolygon );
          if( relate.isEquals( dim1, dim2 ) )
          {
            featurePolygon.setUserData( userData );
            resultGeometryList.add( featurePolygon );
          }
          else if( relate.isIntersects() && !relate.isTouches( dim1, dim2 ) )
          {
            // try intersection
            Geometry intersection = null;
            try
            {
              intersection = featurePolygon.intersection( candidatePolygon );
            }
            catch( final Throwable e )
            {
              try
              {
                // try different way of getting intersection via symmetrical difference
                final Geometry symDifference = candidatePolygon.symDifference( featurePolygon );
                final Geometry part1 = featurePolygon.difference( symDifference );
                final Geometry part2 = candidatePolygon.difference( symDifference );
                intersection = part1.union( part2 );
              }
              catch( final Throwable e2 )
              {
                try
                {
                  intersection = featurePolygon.buffer( 0.001 ).intersection( candidatePolygon.buffer( 0.001 ) );
                }
                catch( final Throwable e3 )
                {
                  // intersection has failed, we cannot use this geometry
                  // calculate error?
                  m_log.add( StatusUtilities.createStatus( IStatus.ERROR, Messages.getString("FeatureListGeometryIntersector.5") + featurePolygon + "\n" + candidatePolygon, e2 ) ); //$NON-NLS-1$ //$NON-NLS-2$
                  continue;
                }
              }
            }

            // discard small (in sum) intersections
            double area = intersection.getArea();
            if( area < MIN_AREA )
            {
              totalAreaDiscarded += area;
              continue;
            }

            // try merging multipart polygons
            try
            {
              intersection = intersection.buffer( 0 );
            }
            catch( final Throwable e )
            {
              m_log.add( StatusUtilities.createStatus( IStatus.WARNING, Messages.getString("FeatureListGeometryIntersector.4") + featurePolygon + "\n" + candidatePolygon, e ) ); //$NON-NLS-1$ //$NON-NLS-2$
            }

            for( int n = 0; n < intersection.getNumGeometries(); n++ )
            {
              final Geometry geometryN = intersection.getGeometryN( n );
              area = geometryN.getArea();
              // discard small parts of multipart polygons
              if( area < MIN_AREA )
              {
                totalAreaDiscarded += area;
                continue;
              }
              geometryN.setUserData( userData );
              resultGeometryList.add( geometryN );
            }

          }
        }
      }
      sourcePolygons = resultGeometryList;
    }
    m_log.add( StatusUtilities.createStatus( IStatus.INFO, Messages.getString("FeatureListGeometryIntersector.9") + totalAreaDiscarded, null ) ); //$NON-NLS-1$
    return resultGeometryList;
  }
}
