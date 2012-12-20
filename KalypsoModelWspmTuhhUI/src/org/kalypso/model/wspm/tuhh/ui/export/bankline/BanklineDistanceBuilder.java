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

import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map.Entry;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.commons.math.geom.PolyLine;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.base.interpolation.FillMissingProfileGeocoordinatesRunnable;
import org.kalypso.model.wspm.tuhh.core.profile.utils.TuhhProfiles;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.algorithm.CGAlgorithms;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.linearref.LengthIndexedLine;

/**
 * @author Gernot Belger
 */
public class BanklineDistanceBuilder
{
  public static enum SIDE
  {
    left,
    right;
  }

  private final IStatusCollector m_log = new StatusCollector( KalypsoModelWspmTuhhUIPlugin.getID() );

  private final SortedMap<Double, Double> m_distances = new TreeMap<>();

  private final LineString m_riverLine;

  private final IProfileFeature[] m_profiles;

  private final LengthIndexedLine m_riverIndex;

  private final SIDE m_side;

  private final IBanklineMarkerProvider m_markerProvider;

  public BanklineDistanceBuilder( final LineString riverLine, final IProfileFeature[] profiles, final IBanklineMarkerProvider markerProvider, final SIDE side )
  {
    m_riverLine = riverLine;
    m_profiles = profiles;
    m_markerProvider = markerProvider;
    m_side = side;

    m_riverIndex = new LengthIndexedLine( m_riverLine );
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
        m_log.add( IStatus.ERROR, Messages.getString( "BanklineDistanceBuilder_0" ), e, profile.getBigStation() ); //$NON-NLS-1$
      }

    }

    final String logMessage = String.format( Messages.getString( "BanklineDistanceBuilder_1" ) ); //$NON-NLS-1$
    return m_log.asMultiStatusOrOK( logMessage, logMessage );
  }

  private void buildDistances( final IProfileFeature profileFeature ) throws Exception
  {
    final IProfile profileCopy = TuhhProfiles.clone( profileFeature.getProfile() );

    final Geometry profileGeometry = JTSAdapter.export( profileFeature.getLine() );
    if( profileGeometry == null || !m_riverLine.intersects( profileGeometry ) )
      return;

    /* Fill missing geo coordinates */
    final FillMissingProfileGeocoordinatesRunnable runnable = new FillMissingProfileGeocoordinatesRunnable( profileCopy );
    m_log.add( runnable.execute( new NullProgressMonitor() ) );

    final String profileSRS = profileFeature.getSrsName();

    /* Optional sanity check */
    if( !m_markerProvider.checkSanity( m_riverLine, profileSRS, profileCopy, m_side ) )
      return;

    /* calculate distances of markers */
    final Coordinate coordinate = m_markerProvider.getMarkerLocation( profileSRS, profileCopy, m_side );
    if( coordinate == null )
      return;

    calculateMarkerDistancePerpendicular( coordinate );
  }

  /**
   * The distance marker to river is calculated as the distance between this point and the river.<br/>
   * The station is the location of the 'lot' of the marker location to the river.
   */
  private void calculateMarkerDistancePerpendicular( final Coordinate markerLocation )
  {
    final double station = m_riverIndex.project( markerLocation );

    final Point markerPoint = m_riverLine.getFactory().createPoint( markerLocation );

    // TODO: get strategy from user?
    final boolean ignoreMarkerOnWrongSide = false;
    if( ignoreMarkerOnWrongSide )
    {
      // REMARK: we are using the signed distance here, to handle cases where the markers
      // are situated on the wrong side of the line (i.e. river line is not between markers).
      final double markerDistance = computeSignedMinDistance( m_riverLine, markerPoint );

      final double sideSign = m_side == SIDE.left ? -1 : +1;
      if( markerDistance * sideSign > 0 )
        m_distances.put( station, Math.abs( markerDistance ) );
    }
    else
    {
      final double markerDistance = m_riverLine.distance( markerPoint );
      m_distances.put( station, markerDistance );
    }
  }

  /**
   * Computes the minimal signed distance between a line and a point.<br/>
   * The absolute value of the returned distance is equal to <code>line.distance( point )</code>.<br/>
   * But the sign of the distance depends of whether the point is to the left or the right of the line.<br/>
   * In particular, the sign is equal to the orientation index returned by
   * {@link CGAlgorithms#orientationIndex(Coordinate, Coordinate, Coordinate)} for the nearest segment of line to the
   * point. <br/>
   * REMARK: this code was mostly copied from JTS v 1.12
   *
   * @see Geometry#distance(Geometry)
   */
  private static double computeSignedMinDistance( final LineString line, final Point pt )
  {
    double absoluteMinDistance = Double.MAX_VALUE;
    double signedMinDistance = Double.NaN;

    final Coordinate[] coord0 = line.getCoordinates();
    final Coordinate coord = pt.getCoordinate();
    // brute force approach!
    for( int i = 0; i < coord0.length - 1; i++ )
    {
      final double dist = CGAlgorithms.distancePointLine( coord, coord0[i], coord0[i + 1] );
      if( dist < absoluteMinDistance )
      {
        absoluteMinDistance = dist;
        // LineSegment seg = new LineSegment( coord0[i], coord0[i + 1] );
        // Coordinate segClosestPoint = seg.closestPoint( coord );
        // locGeom[0] = new GeometryLocation( line, i, segClosestPoint );
        // locGeom[1] = new GeometryLocation( pt, 0, coord );

        final int orientationIndex = CGAlgorithms.orientationIndex( coord0[i], coord0[i + 1], coord );
        signedMinDistance = absoluteMinDistance * orientationIndex;
      }
    }

    return signedMinDistance;
  }

  public PolyLine getDistances( )
  {
    final Set<Entry<Double, Double>> entrySet = m_distances.entrySet();
    final Collection<Point2D> points = new ArrayList<>( m_distances.size() );
    for( final Entry<Double, Double> entry : entrySet )
    {
      final double x = entry.getKey();
      final double y = entry.getValue();
      if( !Double.isNaN( y ) )
        points.add( new Point2D.Double( x, y ) );
    }

    final Point2D[] allPoints = points.toArray( new Point2D[points.size()] );
    if( allPoints.length > 1 )
      return new PolyLine( allPoints, 0.0001 );

    return null;
  }
}