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
package org.kalypso.model.wspm.tuhh.core.profile.importer.wprof;

import java.math.BigDecimal;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.core.profil.IProfilePointPropertyProvider;
import org.kalypso.model.wspm.core.profil.util.ProfileUtil;
import org.kalypso.model.wspm.core.profil.visitors.ProfileVisitors;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.core.KalypsoModelWspmTuhhCorePlugin;
import org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint;
import org.kalypso.observation.result.IRecord;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Gernot Belger
 */
class GelaendeProfileCreator extends AbstractProfileCreator
{
  private final String m_soilPointsID;

  private double m_soilOffset = 0.0;

  private boolean m_isCreateLocation = true;

  public GelaendeProfileCreator( final String description, final ProfileData data, final String soilPolygon )
  {
    super( description, data );

    m_soilPointsID = soilPolygon;
  }

  protected IWProfPoint[] getSoilPoints( )
  {
    final ProfilePolygon profilePolygon = new ProfilePolygon( "-" ); //$NON-NLS-1$

    final IWProfPoint[] soilPoints = getPoints( m_soilPointsID );
    if( soilPoints != null )
    {
      for( final IWProfPoint point : soilPoints )
      {
        profilePolygon.add( point );
      }
    }

    /* We also add points that have no object-type: theses are probably points from the prolongation tool */
    final IWProfPoint[] unknownPoints = getPoints( "" ); //$NON-NLS-1$
    if( unknownPoints != null )
    {
      for( final IWProfPoint point : unknownPoints )
      {
        profilePolygon.add( point );
      }
    }

    return profilePolygon.getPoints();
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.core.profile.importer.wprof.AbstractProfileCreator#configure(org.kalypso.model.wspm.core.profil.IProfil)
   */
  @Override
  protected void configure( final IProfile profile ) throws CoreException
  {
    try
    {
      addSoil( profile );

      addMarker( profile );

      addExtras( profile );
    }
    catch( final Exception e )
    {
      final String message = String.format( "Unable to create profile at %.4f", profile.getStation() ); //$NON-NLS-1$
      final Status status = new Status( IStatus.ERROR, KalypsoModelWspmTuhhCorePlugin.getID(), message, e );
      throw new CoreException( status );
    }
  }

  private void addExtras( final IProfile profile )
  {
    // FIXME: no, use new waterlevel objects instead!
    final Waterlevel2DCreator waterlevelExtra = new Waterlevel2DCreator();
    final IWProfPoint[] soilPoints = getSoilPoints();
    waterlevelExtra.findWaterlevel( soilPoints );
    waterlevelExtra.insertWaterlevel( profile );
    waterlevelExtra.moveDurchstroemteBereiche( profile );
  }

  private void addSoil( final IProfile profile ) throws Exception
  {
    final int hoeheIndex = profile.indexOfProperty( POINT_PROPERTY_HOEHE );
    final int rwIndex = profile.indexOfProperty( POINT_PROPERTY_RECHTSWERT );
    final int hwIndex = profile.indexOfProperty( POINT_PROPERTY_HOCHWERT );
    final int commentIndex = profile.indexOfProperty( POINT_PROPERTY_COMMENT );

    final IWProfPoint[] soilPoints = getSoilPoints();
    for( final IWProfPoint wprofPoint : soilPoints )
    {
      final BigDecimal distance = wprofPoint.getDistance();
      final double value = wprofPoint.getValue();
      final GM_Point location = wprofPoint.getLocation();
      final String comment = wprofPoint.getComment();

      final IRecord point = createPoint( profile, distance );

      final double valuePlusOffset = value + m_soilOffset;
      point.setValue( hoeheIndex, valuePlusOffset );

      if( location != null && m_isCreateLocation )
      {
        final GM_Point transformedLocation = transform( location );
        point.setValue( rwIndex, transformedLocation.getX() );
        point.setValue( hwIndex, transformedLocation.getY() );
      }

      if( comment != null && !comment.isEmpty() )
      {
        point.setValue( commentIndex, comment );
      }
    }
  }

  private IRecord createPoint( final IProfile profil, final BigDecimal distance )
  {
    // Höhe values always get added as new points; we assume that the points are in the right order
    // This is necessary the preserve 'Rücksprünge' in the soil-layer
    final IProfileRecord newPoint = profil.createProfilPoint();
    newPoint.setBreite( new Double( distance.doubleValue() ) );
    profil.addPoint( newPoint );
    return newPoint;
  }

  protected void addMarker( final IProfile profile )
  {
    addMarker( profile, MARKER_TYP_TRENNFLAECHE );
    addMarker( profile, MARKER_TYP_BORDVOLL );
    addMarker( profile, MARKER_TYP_DURCHSTROEMTE );
  }

  protected void addMarker( final IProfile profile, final String markerType )
  {
    final IWProfPoint[] points = getMarkers().getPoints( markerType );
    addMarkers( profile, points, markerType );

    final int numberOfMarkersToAdd = Math.max( 0, 2 - points.length );
    // FIXME: make optional, if Kalypso should automatically add default markers at the end of the profile
    addDefaultMarkers( profile, numberOfMarkersToAdd, markerType );
  }

  private void addDefaultMarkers( final IProfile profile, final int numberOfMarkersToAdd, final String markerType )
  {
    final boolean useLastObservedPoints = false;
    final IProfileRecord firstPoint;
    final IProfileRecord lastPoint;
    if( useLastObservedPoints )
    {
      // TRICKY: we use the soil points, to determine the default points here...
      // This is useful for extended profiles: the markers than sit on the last/first real wprof point.
      final IWProfPoint[] soilPoints = getPoints( m_soilPointsID );

      if( soilPoints.length < 2 )
        return;

      final IWProfPoint firstSoilPoint = soilPoints[0];
      final IWProfPoint lastSoilPoint = soilPoints[soilPoints.length - 1];

      firstPoint = ProfileUtil.findPoint( profile, firstSoilPoint.getDistance().doubleValue(), 0.0001 );
      lastPoint = ProfileUtil.findPoint( profile, lastSoilPoint.getDistance().doubleValue(), 0.0001 );
    }
    else
    {
      final IProfileRecord[] points = profile.getPoints();
      if( points.length < 2 )
        return;

      firstPoint = points[0];
      lastPoint = points[points.length - 1];
    }

    switch( numberOfMarkersToAdd )
    {
      case 1:
        createMarkers( profile, new IProfileRecord[] { lastPoint }, markerType );
        break;

      case 2:
        createMarkers( profile, new IProfileRecord[] { firstPoint, lastPoint }, markerType );
        break;

      case 0:
        return;

      default:
        throw new IllegalArgumentException( "numberOfMarkersToAdd must be less than 2" ); //$NON-NLS-1$
    }
  }

  protected void addMarkers( final IProfile profile, final IWProfPoint[] points, final String markerType )
  {
    final IProfileRecord[] pointsToMark = findPoints( profile, points );
    createMarkers( profile, pointsToMark, markerType );
  }

  protected void createMarkers( final IProfile profile, final IProfileRecord[] points, final String markerType )
  {
    final IProfilePointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profile.getType() );

    for( final IProfileRecord point : points )
    {
      final IProfilePointMarker marker = profile.createPointMarker( markerType, point );
      final Object defaultValue = provider.getDefaultValue( markerType );
      marker.setValue( defaultValue );
    }
  }

  private IProfileRecord[] findPoints( final IProfile profile, final IWProfPoint[] points )
  {
    final IProfileRecord[] result = new IProfileRecord[points.length];
    for( int i = 0; i < result.length; i++ )
    {
      result[i] = findPoint( profile, points[i] );
    }

    return result;
  }

  private IProfileRecord findPoint( final IProfile profile, final IWProfPoint wProfPoint )
  {
    final BigDecimal distance = wProfPoint.getDistance();
    return ProfileVisitors.findNearestPoint( profile, distance.doubleValue() );
  }

  public void setSoilOffset( final double offset )
  {
    m_soilOffset = offset;
  }

  public void setCreateLocation( final boolean isCreateLocation )
  {
    m_isCreateLocation = isCreateLocation;
  }

}
