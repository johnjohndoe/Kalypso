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
package org.kalypso.model.wspm.tuhh.ui.export.bankline;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.SortedMap;
import java.util.TreeMap;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.base.FillMissingProfileGeocoordinatesRunnable;
import org.kalypso.model.wspm.core.profil.wrappers.ProfilePointWrapper;
import org.kalypso.model.wspm.core.profil.wrappers.ProfileWrapper;
import org.kalypso.model.wspm.tuhh.core.profile.utils.TuhhProfiles;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.transformation.transformer.JTSTransformer;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;

/**
 * @author Gernot Belger
 */
public class BanklineDistanceBuilder
{
  private final IStatusCollector m_log = new StatusCollector( KalypsoModelWspmTuhhUIPlugin.getID() );

  private final SortedMap<Double, BanklineDistances> m_distances = new TreeMap<>();

  private final LineString m_riverLine;

  private final IProfileFeature[] m_profiles;

  public BanklineDistanceBuilder( final LineString riverLine, final IProfileFeature[] profiles )
  {
    m_riverLine = riverLine;
    m_profiles = profiles;
  }

  public SortedMap<Double, BanklineDistances> getDistances( )
  {
    return Collections.unmodifiableSortedMap( m_distances );
  }

  public IStatus execute( )
  {
    for( final IProfileFeature profile : m_profiles )
    {
      try
      {
        buildDistances( profile );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        m_log.add( IStatus.ERROR, "Failed to calculate distances for cross section '%s'", e, profile.getBigStation() );
      }

    }

    final String logMessage = String.format( "Determine bank line distances" );
    return m_log.asMultiStatusOrOK( logMessage, logMessage );
  }

  private void buildDistances( final IProfileFeature profileFeature ) throws Exception
  {
    final IProfil profileCopy = TuhhProfiles.clone( profileFeature.getProfil() );

    /* Cross section geometry */
    final GM_Curve line = profileFeature.getLine();
    final LineString crossSection = (LineString) JTSAdapter.export( line );
    if( crossSection == null || crossSection.getNumPoints() < 2 )
    {
      m_log.add( IStatus.WARNING, "Invalid geometry for for cross section '%s'", null, profileFeature.getBigStation() );
      return;
    }

    /* Intersect with river */
    final Geometry intersection = crossSection.intersection( m_riverLine );
    final Point[] intersections = findPoints( intersection );
    if( intersections.length == 0 )
    {
      final double distance = crossSection.distance( m_riverLine );
      m_log.add( IStatus.WARNING, "Cross section '%s' does not intersect with center line (distance is %.2f [m])", null, profileFeature.getBigStation(), distance );
      return;
    }

    if( intersections.length > 1 )
    {
      final double distance = crossSection.distance( m_riverLine );
      m_log.add( IStatus.WARNING, "Cross section '%s' intersect with center line more than once.", null, profileFeature.getBigStation(), distance );
      return;
    }

    /* Find station in river line */
    final Point intersectionPoint = intersections[0];
    final double station = JTSUtilities.pointDistanceOnLine( m_riverLine, intersectionPoint );

    final BanklineDistances banklineDistances = new BanklineDistances( station );
    m_distances.put( station, banklineDistances );

    /* Fill missing geo coordinates */
    final FillMissingProfileGeocoordinatesRunnable runnable = new FillMissingProfileGeocoordinatesRunnable( new ProfileWrapper( profileCopy ) );
    m_log.add( runnable.execute( new NullProgressMonitor() ) );

    final String profileSRS = profileFeature.getSrsName();
    final String kalypsoSRS = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

    final int profileSRID = JTSAdapter.toSrid( profileSRS );
    final int kalypsoSRID = JTSAdapter.toSrid( kalypsoSRS );

    final JTSTransformer jtsTransformer = new JTSTransformer( profileSRID, kalypsoSRID );

    /* calculate distances of markers */
    final IComponent[] markerTypes = profileCopy.getPointMarkerTypes();
    for( final IComponent markerType : markerTypes )
    {
      final IProfilPointMarker[] pointMarkers = profileCopy.getPointMarkerFor( markerType );
      for( int i = 0; i < pointMarkers.length; i++ )
      {
        final IProfilPointMarker pointMarker = pointMarkers[i];
        final IRecord point = pointMarker.getPoint();

        final ProfilePointWrapper pointWrapper = new ProfilePointWrapper( point );
        final Coordinate coordinate = jtsTransformer.transform( pointWrapper.getCoordinate() );

        final Point markerLocation = m_riverLine.getFactory().createPoint( coordinate );

        final String markerName = String.format( "%s_%d", pointMarker.getComponent().getId(), i );

        final double markerDistance = markerLocation.distance( intersectionPoint );
        banklineDistances.setDistance( markerName, markerDistance );
      }
    }
  }

  private static Point[] findPoints( final Geometry intersection )
  {
    if( intersection instanceof Point )
      return new Point[] { (Point) intersection };

    if( intersection instanceof GeometryCollection )
    {
      final Collection<Point> points = new ArrayList<>();
      final GeometryCollection collection = (GeometryCollection) intersection;
      final int numGeometries = collection.getNumGeometries();
      for( int i = 0; i < numGeometries; i++ )
      {
        final Geometry geom = collection.getGeometryN( i );
        if( geom instanceof Point )
          points.add( (Point) geom );
      }

      return points.toArray( new Point[points.size()] );
    }

    return new Point[] {};
  }
}