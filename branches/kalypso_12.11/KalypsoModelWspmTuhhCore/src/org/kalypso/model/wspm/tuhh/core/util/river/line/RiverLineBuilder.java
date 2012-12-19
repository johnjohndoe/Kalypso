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
package org.kalypso.model.wspm.tuhh.core.util.river.line;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.commons.lang3.tuple.Pair;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.core.profil.visitors.ProfileVisitors;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.core.util.WspmProfileHelper;
import org.kalypso.transformation.transformer.JTSTransformer;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.opengis.referencing.FactoryException;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.math.Vector2D;

/**
 * Derives a river line (centerline, bankline, ...) from a set of objects.
 * 
 * @author Gernot Belger
 */
public class RiverLineBuilder
{
  private final IProfileFeature[] m_profiles;

  /**
   * @param profiles
   *          , must be sorted in flow direction
   */
  public RiverLineBuilder( final IProfileFeature[] profiles )
  {
    m_profiles = profiles;
  }

  public LineString execute( )
  {
    final Coordinate[] coordinates = buildCoordinates();

    final Coordinate[] interpolatedCrds = interpolateRiverLine( coordinates );

    final GeometryFactory factory = new GeometryFactory();
    return factory.createLineString( interpolatedCrds );
  }

  private Coordinate[] buildCoordinates( )
  {
    final List<Pair<Coordinate, Vector2D>> directedLowPoints = new ArrayList<>( m_profiles.length );

    /* Find low points including direction */
    for( final IProfileFeature profileFeature : m_profiles )
    {
      final Pair<Coordinate, Vector2D> locationAndDirction = getLocationAndDirction( profileFeature );
      directedLowPoints.add( locationAndDirction );
    }

    /* add intermediate points: in order to keep direction of river line orthogonal to cross section */
    final Collection<Coordinate> crds = new ArrayList<>( directedLowPoints.size() * 3 );

    // TODO: very poor approximation if an arc two adjacent cross sections
    // we should use a proper arc implementation

    for( int i = 0; i < directedLowPoints.size(); i++ )
    {
      final Coordinate prevLocation = i < 1 ? null : directedLowPoints.get( i - 1 ).getKey();
      final Pair<Coordinate, Vector2D> current = directedLowPoints.get( i );
      final Coordinate nextLocation = i > directedLowPoints.size() - 2 ? null : directedLowPoints.get( i + 1 ).getKey();

      final Coordinate currentLocation = current.getKey();
      final Vector2D currentDirection = current.getValue();

      if( currentDirection != null && prevLocation != null )
      {
        final double prevDistance = prevLocation.distance( currentLocation );

        final Vector2D shift = currentDirection.multiply( prevDistance / -10.0 );

        crds.add( shift.translate( currentLocation ) );
      }

      crds.add( currentLocation );

      if( currentDirection != null && nextLocation != null )
      {
        final double nextDistance = nextLocation.distance( currentLocation );

        final Vector2D shift = currentDirection.multiply( nextDistance / 10.0 );
        crds.add( shift.translate( currentLocation ) );
      }
    }

    return crds.toArray( new Coordinate[crds.size()] );
  }

  private Pair<Coordinate, Vector2D> getLocationAndDirction( final IProfileFeature profileFeature )
  {
    try
    {
      final IProfile profil = profileFeature.getProfile();

      /* Prepare for transformation */
      final String profileSRS = profileFeature.getSrsName();
      final String kalypsoSRS = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

      final int profileSRID = JTSAdapter.toSrid( profileSRS );
      final int kalypsoSRID = JTSAdapter.toSrid( kalypsoSRS );

      final JTSTransformer jtsTransformer = new JTSTransformer( profileSRID, kalypsoSRID );

      final Coordinate lowLocation = findLowPoint( profil, jtsTransformer );
      if( lowLocation == null )
        return null;

      final Vector2D direction = findDirection( profil, jtsTransformer );

      return Pair.of( lowLocation, direction );
    }
    catch( final FactoryException e )
    {
      // FIXME: what to do with the exceptions?
      e.printStackTrace();
      return null;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  private Coordinate findLowPoint( final IProfile profil, final JTSTransformer transformer ) throws Exception
  {
    try
    {
      final double width = WspmSohlpunkte.findSohlpunkt( profil );
      final GM_Point gmp = WspmProfileHelper.getGeoPositionKalypso( width, profil );
      final Point point = (Point) JTSAdapter.export( gmp );

      return transformer.transform( point.getCoordinate() );
    }
    catch( final Throwable t )
    {
      // nothing to do
    }

    // fallback
    final IProfileRecord lowestPoint = ProfileVisitors.findLowestPoint( profil );
    if( lowestPoint == null )
      return null;

    final Coordinate lowLocation = lowestPoint.getCoordinate();
    return transformer.transform( lowLocation );
  }

  private Vector2D findDirection( final IProfile profil, final JTSTransformer transformer ) throws Exception
  {
    // IProfileRecord[] pointsForDirection = profil.getPoints();
    final IProfilePointMarker[] markerPoints = profil.getPointMarkerFor( "urn:ogc:gml:dict:kalypso:model:wspm:profileMarkerComponents#TRENNFLAECHE" ); //$NON-NLS-1$
    final IProfileRecord[] pointsForDirection = new IProfileRecord[markerPoints.length];
    for( int i = 0; i < pointsForDirection.length; i++ )
      pointsForDirection[i] = markerPoints[i].getPoint();

    if( pointsForDirection.length < 2 )
      return null;

    final IProfileRecord first = pointsForDirection[0];
    final IProfileRecord last = pointsForDirection[pointsForDirection.length - 1];

    if( first == null || last == null )
      return null;

    final Coordinate firstCoordinate = transformer.transform( first.getCoordinate() );
    final Coordinate lastCoordinate = transformer.transform( last.getCoordinate() );

    return Vector2D.create( firstCoordinate, lastCoordinate ).normalize().rotateByQuarterCircle( 3 );
  }

  private Coordinate[] interpolateRiverLine( final Coordinate[] coordinates )
  {
    return coordinates;
  }
}