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
package org.kalypso.model.rcm.util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.model.rcm.binding.IOmbrometer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.kalypsodeegree_impl.model.sort.IEnvelopeProvider;
import org.kalypsodeegree_impl.model.sort.SplitSort;
import org.openjump.core.graph.delauneySimplexInsert.DTriangulationForJTS;

import com.vividsolutions.jts.algorithm.ConvexHull;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Utilities for {@link org.kalypso.model.rcm.binding.IOmbrometer}'s.
 * 
 * @author Gernot Belger
 */
public class OmbrometerUtils
{
  /**
   * This special envelope provider is used for the geo-index on the ombrometer stations.<br>
   * It only considers its stationLocations.
   */
  public static final IEnvelopeProvider OMBROMETER_ENVELOPE_PROVIDER = new IEnvelopeProvider()
  {
    @Override
    public GM_Envelope getEnvelope( final Object o )
    {
      final IOmbrometer ombrometer = (IOmbrometer) o;
      return ombrometer.getStationLocation().getEnvelope();
    }
  };

  private OmbrometerUtils( )
  {
    // helper class, do not instantiate
  }

  /**
   * Calculates thiessen polygons for a list of ombrometers.<br>
   * Only omrbometers, whichs 'isUsed' flag is set are considered.<br>
   * The thiessen polygons are intersected with a buffer of the convex hull of all ombrometers<br>
   * 
   * @param ombrometerList
   *          List of features of ombrometers.
   * @param bufferRatio
   *          Determines the size of the buffer; its the ratio of the size of the convex hull that is added to the
   *          buffer.
   */
  public static Map<IOmbrometer, GM_Surface<GM_SurfacePatch>> thiessenPolygons( final List< ? > ombrometerList, final double bufferRatio, final IProgressMonitor monitor ) throws CoreException, GM_Exception
  {
    monitor.beginTask( "Thiessen Polygone ermitteln", 6 );

    if( ombrometerList.isEmpty() )
      return Collections.emptyMap();

    final Feature parentFeature = ((Feature) ombrometerList.get( 0 )).getParent();
    final IRelationType parentRelation = ((Feature) ombrometerList.get( 0 )).getParentRelation();

    // Gather data:
    // - used point into point-list
    // - all coordinates into for convex hull
    // - ombrometer into geo index for quicker search later
    // - feature changes for ombrometers to null, in order to delete old geometries
    final List<com.vividsolutions.jts.geom.Point> points = new ArrayList<com.vividsolutions.jts.geom.Point>();
    final SplitSort geoIndex = new SplitSort( parentFeature, parentRelation, OMBROMETER_ENVELOPE_PROVIDER );
    final Map<IOmbrometer, GM_Surface<GM_SurfacePatch>> changeMap = new HashMap<IOmbrometer, GM_Surface<GM_SurfacePatch>>();
    final List<Coordinate> crds = new ArrayList<Coordinate>();
    for( final Object listEntry : ombrometerList )
    {
      final IOmbrometer ombro = (IOmbrometer) ((Feature) listEntry).getAdapter( IOmbrometer.class );
      final GM_Point stationLocation = ombro.getStationLocation();
      final com.vividsolutions.jts.geom.Point point = (com.vividsolutions.jts.geom.Point) JTSAdapter.export( stationLocation );
      if( ombro.isUsed() )
      {
        points.add( point );
        geoIndex.add( ombro );
      }
      changeMap.put( ombro, null );
      crds.add( point.getCoordinate() );
    }

    ProgressUtilities.worked( monitor, 1 );

    if( points.size() >= 2 )
    {
      // Calculate voronoi polygons ;only if we have at least 2 points.
      // Else, all areas will get deleted

      // Convex hull of points as boundary of thiessen
      final ConvexHull convexHull = new ConvexHull( crds.toArray( new Coordinate[crds.size()] ), new GeometryFactory() );
      final Geometry boundary = convexHull.getConvexHull();

      ProgressUtilities.worked( monitor, 2 );

      final Geometry thiessenBoundary = JTSUtilities.bufferWithRatio( boundary, bufferRatio );
      final DTriangulationForJTS tri = new DTriangulationForJTS( points, thiessenBoundary );
      final List<Polygon> thiessenPolys = tri.getThiessenPolys();

      ProgressUtilities.worked( monitor, 2 );

      for( final Polygon polygon : thiessenPolys )
      {
        final GM_Surface<GM_SurfacePatch> affectedArea = (GM_Surface<GM_SurfacePatch>) JTSAdapter.wrap( polygon );
        final IOmbrometer ombro = findOmbrometerFor( affectedArea, geoIndex );
        if( ombro == null )
          throw new GM_Exception( "Fehler bei der Ermittlung der Thiessen Polygone" );
        else
          changeMap.put( ombro, affectedArea );
      }

      monitor.worked( 1 );
    }
    else
      ProgressUtilities.worked( monitor, 5 );

    return changeMap;
  }

  private static IOmbrometer findOmbrometerFor( final GM_Surface<GM_SurfacePatch> surface, final SplitSort geoIndex )
  {
    final List< ? > query = geoIndex.query( surface.getEnvelope(), null );
    for( final Object object : query )
    {
      final IOmbrometer ombro = (IOmbrometer) object;
      if( surface.contains( ombro.getStationLocation() ) )
        return ombro;
    }

    return null;
  }

}
