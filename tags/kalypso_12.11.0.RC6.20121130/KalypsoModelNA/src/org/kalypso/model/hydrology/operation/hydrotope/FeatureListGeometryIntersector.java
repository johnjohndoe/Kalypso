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
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree_impl.model.sort.SpatialIndexExt;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.IntersectionMatrix;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.util.PolygonExtracter;

/**
 * Utility class for intersecting a number of feature geometry layers<br/>
 * Intersects features given as a list of feature lists, produces the result into the resultList<br>
 * <br/>
 * refactored by Stefan Kurzbach <br/>
 * commented out some code for fixing bad geometries
 * 
 * @author Dejan Antanaskovic
 */
class FeatureListGeometryIntersector implements ICoreRunnableWithProgress
{
  static final double MIN_AREA = 0.001;

  private final List<Polygon> m_resultGeometryList = new ArrayList<>();

  double m_totalAreaDiscarded = 0;

  private final String m_logLabel;

  private final IHydrotopeInput[] m_input;

  public FeatureListGeometryIntersector( final IHydrotopeInput[] input, final String logLabel )
  {
    m_input = input;
    m_logLabel = logLabel;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final IStatusCollector log = new StatusCollector( ModelNA.PLUGIN_ID );

    final String taskName = Messages.getString( "org.kalypso.convert.namodel.FeatureListGeometryIntersector.0" ); //$NON-NLS-1$

    final SubMonitor progress = SubMonitor.convert( monitor, taskName, 100 );

    /* Do the intersection */
    final SpatialIndexExt sourceIndex = m_input[0].getIndex();

    final List< ? > sourcePolygons = sourceIndex.query( sourceIndex.getBoundingBox() );
    final int countSourcePolygons = sourcePolygons.size();

    progress.setWorkRemaining( countSourcePolygons );

    for( int i = 0; i < countSourcePolygons; i++ )
    {
      ProgressUtilities.workedModulo( monitor, i, countSourcePolygons, 13, Messages.getString( "org.kalypso.convert.namodel.FeatureListGeometryIntersector.2" ) ); //$NON-NLS-1$

      final Polygon sourcePolygon = (Polygon) sourcePolygons.get( i );

      intersectSource( sourcePolygon, m_input, log );
    }
    log.add( IStatus.INFO, Messages.getString( "FeatureListGeometryIntersector.9", m_totalAreaDiscarded ), null ); //$NON-NLS-1$

    return log.asMultiStatus( m_logLabel );
  }

  private void intersectSource( final Polygon sourcePolygon, final IHydrotopeInput[] input, final IStatusCollector log )
  {
    List<Polygon> currentIntersections = Collections.singletonList( sourcePolygon );

    for( int j = 1; j < input.length; j++ )
    {
      final SpatialIndexExt targetIndex = input[j].getIndex();

      final List<Polygon> newIntersections = new ArrayList<>();

      for( final Polygon source : currentIntersections )
      {
        findIntersections( source, targetIndex, log, newIntersections );
      }

      currentIntersections = newIntersections;
    }

    /* Add all intersections as to result set */
    m_resultGeometryList.addAll( currentIntersections );
  }

  private void findIntersections( final Polygon sourcePolygon, final SpatialIndexExt targetIndex, final IStatusCollector log, final List<Polygon> result )
  {
    final HydrotopeUserData sourceData = (HydrotopeUserData) sourcePolygon.getUserData();
    final int dim1 = sourcePolygon.getDimension();

    final List<Polygon> targetPolygons = targetIndex.query( sourcePolygon.getEnvelopeInternal() );

    for( final Polygon targetPolygon : targetPolygons )
    {
      final HydrotopeUserData targetData = (HydrotopeUserData) targetPolygon.getUserData();

      final int dim2 = targetPolygon.getDimension();

      final IntersectionMatrix relate = sourcePolygon.relate( targetPolygon );
      if( relate.isEquals( dim1, dim2 ) )
      {
        final HydrotopeUserData mergedData = sourceData.merge( targetData, targetPolygon );
        sourcePolygon.setUserData( mergedData );

        addResult( result, sourcePolygon );
      }
      else if( relate.isIntersects() && !relate.isTouches( dim1, dim2 ) )
      {
        Geometry intersection = forceIntersection( sourcePolygon, targetPolygon, log );

        try
        {
          intersection = intersection.buffer( 0 );
        }
        catch( final Throwable e )
        {
          final String sourceLabel = ((HydrotopeUserData) sourcePolygon.getUserData()).toErrorString();
          final String targetLabel = ((HydrotopeUserData) targetPolygon.getUserData()).toErrorString();

          log.add( IStatus.WARNING, Messages.getString( "FeatureListGeometryIntersector.4", sourceLabel, targetLabel ), e ); //$NON-NLS-1$ //$NON-NLS-2$
        }

        final List<Polygon> intersectionPolygons = PolygonExtracter.getPolygons( intersection );
        final Polygon[] polygons = intersectionPolygons.toArray( new Polygon[intersectionPolygons.size()] );
        for( final Polygon polygon : polygons )
        {
          final HydrotopeUserData mergedData = sourceData.merge( targetData, polygon );
          polygon.setUserData( mergedData );

          addResult( result, polygon );
        }
      }
    }
  }

  private void addResult( final List<Polygon> result, final Polygon polygon )
  {
    // discard small (in sum) intersections
    final double area = polygon.getArea();
    if( area < MIN_AREA )
    {
      m_totalAreaDiscarded += area;
      return;
    }

    result.add( polygon );
  }

  static Geometry forceIntersection( final Polygon sourcePolygon, final Polygon targetPolygon, final IStatusCollector log )
  {
    // try intersection
    try
    {
      return sourcePolygon.intersection( targetPolygon );
    }
    catch( final Throwable e )
    {
      try
      {
        // try different way of getting intersection via symmetrical difference
        final Geometry symDifference = targetPolygon.symDifference( sourcePolygon );
        final Geometry part1 = sourcePolygon.difference( symDifference );
        final Geometry part2 = targetPolygon.difference( symDifference );
        return part1.union( part2 );
      }
      catch( final Throwable e2 )
      {
        try
        {
          // TODO: why 0.001?
          return sourcePolygon.buffer( 0.001 ).intersection( targetPolygon.buffer( 0.001 ) );
        }
        catch( final Throwable e3 )
        {
          // intersection has failed, we cannot use this geometry
          // calculate error?
          final String sourceLabel = ((HydrotopeUserData) sourcePolygon.getUserData()).toErrorString();
          final String targetLabel = ((HydrotopeUserData) targetPolygon.getUserData()).toErrorString();

          final String message = Messages.getString( "FeatureListGeometryIntersector.5", sourceLabel, targetLabel ); //$NON-NLS-1$
          if( log != null )
            log.add( IStatus.ERROR, message, e2 );
          return null;
        }
      }
    }
  }

  public List<Polygon> getResult( )
  {
    return Collections.unmodifiableList( m_resultGeometryList );
  }
}