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
package org.kalypso.model.rcm.util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.kalypsodeegree_impl.model.sort.IEnvelopeProvider;
import org.openjump.core.graph.delauneySimplexInsert.DTriangulationForJTS;

import com.vividsolutions.jts.algorithm.ConvexHull;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Calculates the thiessen areas (polygons) from a set of stations.
 * 
 * @author Gernot Belger
 */
public class ThiessenAreaOperation
{
  private final QName m_propertyLocation;

  private final QName m_propertyActive;

  /**
   * This special envelope provider is used for the geo-index on the ombrometer stations.<br>
   * It only considers its stationLocations.
   */
  private IEnvelopeProvider m_envelopeProvider;

  public ThiessenAreaOperation( final QName propertyLocation, final QName propertyActive )
  {
    m_propertyLocation = propertyLocation;
    m_propertyActive = propertyActive;

    m_envelopeProvider = new IEnvelopeProvider()
    {
      @Override
      public GM_Envelope getEnvelope( final Object o )
      {
        final Feature ombrometer = (Feature)o;
        final GM_Point stationLocation = (GM_Point)ombrometer.getProperty( propertyLocation );
        return stationLocation.getEnvelope();
      }
    };
  }

  /**
   * Calculates thiessen polygons for a list of ombrometers.<br>
   * Only omrbometers, whichs 'isUsed' flag is set are considered.<br>
   * The thiessen polygons are intersected with a buffer of the convex hull of all ombrometers<br>
   * <br>
   * The thiessen method does work only, if at least 2 point are available. In order to handle one more case (1
   * available point), the whole buffer region is used in case of only one available point.<br>
   * 
   * @param ombrometerList
   *          List of features of ombrometers.
   * @param boundaryCalculator
   *          Determines the area that will be devided in thiessen polygons
   */
  public Map<Feature, GM_Polygon> execute( final List< ? > stations, final IBoundaryCalculator boundaryCalculator, final IProgressMonitor monitor ) throws CoreException, GM_Exception
  {
    monitor.beginTask( "Thiessen Polygone ermitteln", 6 );

    if( stations.isEmpty() )
      return Collections.emptyMap();

    final Feature parentFeature = ((Feature)stations.get( 0 )).getOwner();
    final IRelationType parentRelation = ((Feature)stations.get( 0 )).getParentRelation();

    // Gather data:
    // - used point into point-list
    // - all coordinates into list for convex hull
    // - ombrometer into geo index for quicker search later
    // - feature changes for ombrometers to null, in order to delete old geometries
    final List<com.vividsolutions.jts.geom.Point> points = new ArrayList<>();
    final FeatureList geoIndex = FeatureFactory.createFeatureList( parentFeature, parentRelation, m_envelopeProvider );
    final Map<Feature, GM_Polygon> changeMap = new HashMap<>();
    final List<Coordinate> crds = new ArrayList<>();
    String crs = null;
    for( final Object listEntry : stations )
    {
      final Feature ombro = (Feature)listEntry;
      final GM_Point stationLocation = (GM_Point)ombro.getProperty( m_propertyLocation );
      if( stationLocation != null )
      {
        final com.vividsolutions.jts.geom.Point point = (com.vividsolutions.jts.geom.Point)JTSAdapter.export( stationLocation );
        final Boolean isActive = (Boolean)ombro.getProperty( m_propertyActive );
        if( isActive != null && isActive )
        {
          crs = stationLocation.getCoordinateSystem();
          points.add( point );
          geoIndex.add( ombro );
        }
        crds.add( point.getCoordinate() );
      }
      changeMap.put( ombro, null );
    }

    ProgressUtilities.worked( monitor, 1 );

    // Convex hull of points as boundary for thiessen
    final ConvexHull convexHull = new ConvexHull( crds.toArray( new Coordinate[crds.size()] ), new GeometryFactory() );
    final Geometry boundary = convexHull.getConvexHull();

    ProgressUtilities.worked( monitor, 1 );

    final Geometry thiessenBoundary = boundaryCalculator.calculateBoundary( boundary );
    ProgressUtilities.worked( monitor, 1 );

    if( geoIndex.size() == 0 )
    {

    }
    else if( geoIndex.size() == 1 )
    {
      final Feature ombro = (Feature)geoIndex.get( 0 );
      final GM_Polygon gmBoundary = (GM_Polygon)JTSAdapter.wrap( thiessenBoundary, crs );
      gmBoundary.setCoordinateSystem( crs );
      changeMap.put( ombro, gmBoundary );
    }
    else
    {
      // Calculate voronoi polygons
      final DTriangulationForJTS tri = new DTriangulationForJTS( points, thiessenBoundary );
      // tri.getAllTrianglesWithZValues( ring, crs )
      final List<Polygon> thiessenPolys = tri.getThiessenPolys();
      ProgressUtilities.worked( monitor, 2 );

      for( final Polygon polygon : thiessenPolys )
      {
        final GM_Polygon affectedArea = (GM_Polygon)JTSAdapter.wrap( polygon, crs );
        final Feature ombro = findOmbrometerFor( affectedArea, geoIndex, m_propertyLocation );
        if( ombro == null )
          throw new GM_Exception( "Fehler bei der Ermittlung der Thiessen Polygone" );
        else
          changeMap.put( ombro, affectedArea );
      }

      monitor.worked( 1 );
    }

    ProgressUtilities.done( monitor );

    return changeMap;
  }

  private static Feature findOmbrometerFor( final GM_Polygon surface, final FeatureList geoIndex, final QName propertyLocation )
  {
    @SuppressWarnings( "unchecked" ) final List< ? > query = geoIndex.query( surface.getEnvelope(), null );
    for( final Object object : query )
    {
      final Feature ombro = (Feature)object;
      final GM_Point location = (GM_Point)ombro.getProperty( propertyLocation );
      if( surface.contains( location ) )
        return ombro;
    }

    return null;
  }
}
