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
package org.kalypso.model.wspm.tuhh.core.profile.importer.wprof;

import java.math.BigDecimal;
import java.net.URL;

import org.apache.commons.io.FilenameUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.KalypsoModelWspmTuhhCorePlugin;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint;
import org.kalypso.observation.result.IRecord;
import org.kalypso.transformation.GeoTransformer;
import org.kalypsodeegree_impl.gml.binding.commons.Image;

/**
 * @author Gernot Belger
 */
class ProfileCreator implements IWspmTuhhConstants
{
  private final ProfilePolygones m_polygones;

  private final GeoTransformer m_transformer;

  private final ProfileDataAdderFactory m_dataAdderFactory;

  private final ProfileMarkers m_markers;

  public ProfileCreator( final ProfilePolygones polygones, final ProfileMarkers markers, final GeoTransformer transformer )
  {
    m_polygones = polygones;
    m_markers = markers;
    m_transformer = transformer;
    m_dataAdderFactory = new ProfileDataAdderFactory( polygones );
  }

  public void createProfile( final TuhhWspmProject project ) throws CoreException
  {
    final IProfileFeature profileFeature = createNewProfile( project );
    if( profileFeature == null )
      return;

    final IProfil profile = profileFeature.getProfil();

    try
    {
      addData( profile );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();

      final IStatus status = e.getStatus();
      final String message = StatusUtilities.messageFromStatus( status );
      profile.setComment( message );
    }

    ProfileFeatureFactory.toFeature( profile, profileFeature );
  }

  private void addData( final IProfil profile ) throws CoreException
  {
    addBasicData( profile );

    final IProfileDataAdder dataAdder = m_dataAdderFactory.createDataAdder();
    if( dataAdder != null )
    {
      dataAdder.configure( profile, m_transformer );
      addMarker( profile );
    }
  }

  private void addBasicData( final IProfil profile )
  {
    final IWProfPoint wprofPoint = m_polygones.getAnyPoint();

    final String riverId = wprofPoint.getRiverId();
    final BigDecimal station = wprofPoint.getStation();
    final String comment = wprofPoint.getProfileComment();
    final String profileName = wprofPoint.getProfileName();

    profile.setName( profileName );
    profile.setComment( comment );
    profile.setStation( station.doubleValue() );
    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profile.getType() );
    profile.addPointProperty( provider.getPointProperty( POINT_PROPERTY_RECHTSWERT ) );
    profile.addPointProperty( provider.getPointProperty( POINT_PROPERTY_HOCHWERT ) );
    profile.addPointProperty( provider.getPointProperty( POINT_PROPERTY_COMMENT ) );

    System.out.println( String.format( "Create profile for riverId '%s' at station '%f'", riverId, station ) ); //$NON-NLS-1$
  }

  private IProfileFeature createNewProfile( final TuhhWspmProject project ) throws CoreException
  {
    final IWProfPoint anyPoint = m_polygones.getAnyPoint();
    if( anyPoint == null )
      return null;

    final BigDecimal station = anyPoint.getStation();

    try
    {
      final String riverId = anyPoint.getRiverId();
      final URL[] photoUrls = anyPoint.getPhotos();

      final IProfileFeature profileFeature = project.createNewProfile( riverId, true );
      profileFeature.setSrsName( m_transformer.getTarget() );
      for( final URL url : photoUrls )
      {
        final Image image = profileFeature.addImage( url );
        final String path = url.getPath();
        final String imageName = FilenameUtils.getBaseName( path );
        image.setName( imageName );
      }

      return profileFeature;
    }
    catch( final GMLSchemaException e )
    {
      final String message = String.format( "Unable to create profile at %s", station ); //$NON-NLS-1$
      final Status status = new Status( IStatus.ERROR, KalypsoModelWspmTuhhCorePlugin.getID(), message, e );
      throw new CoreException( status );
    }
  }

  private void addMarker( final IProfil profile )
  {
    addMarker( profile, MARKER_TYP_TRENNFLAECHE );
    addMarker( profile, MARKER_TYP_BORDVOLL );
    addMarker( profile, MARKER_TYP_DURCHSTROEMTE );
  }

  private void addMarker( final IProfil profile, final String markerType )
  {
    final IWProfPoint[] points = m_markers.getPoints( markerType );
    addMarkers( profile, points, markerType );

    final int numberOfMarkersToAdd = Math.max( 0, 2 - points.length );
    addDefaultMarkers( profile, numberOfMarkersToAdd, markerType );
  }

  private void addDefaultMarkers( final IProfil profile, final int numberOfMarkersToAdd, final String markerType )
  {
    final IRecord[] points = profile.getPoints();
    if( points.length < 2 )
      return;

    final IRecord firstPoint = points[0];
    final IRecord lastPoint = points[points.length - 1];

    switch( numberOfMarkersToAdd )
    {
      case 1:
        createMarkers( profile, new IRecord[] { lastPoint }, markerType );
        break;

      case 2:
        createMarkers( profile, new IRecord[] { firstPoint, lastPoint }, markerType );
        break;

      case 0:
        return;

      default:
        throw new IllegalArgumentException( "numberOfMarkersToAdd must be less than 2" );
    }
  }

  private void addMarkers( final IProfil profile, final IWProfPoint[] points, final String markerType )
  {
    final IRecord[] pointsToMark = findPoints( profile, points );
    createMarkers( profile, pointsToMark, markerType );
  }

  private IRecord[] findPoints( final IProfil profile, final IWProfPoint[] points )
  {
    final IRecord[] result = new IRecord[points.length];
    for( int i = 0; i < result.length; i++ )
      result[i] = findPoint( profile, points[i] );

    return result;
  }

  private IRecord findPoint( final IProfil profile, final IWProfPoint wProfPoint )
  {
    final BigDecimal distance = wProfPoint.getDistance();
    return ProfilUtil.findNearestPoint( profile, distance.doubleValue() );
  }

  private void createMarkers( final IProfil profile, final IRecord[] points, final String markerType )
  {
    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profile.getType() );

    for( final IRecord point : points )
    {
      final IProfilPointMarker marker = profile.createPointMarker( markerType, point );
      final Object defaultValue = provider.getDefaultValue( markerType );
      marker.setValue( defaultValue );
    }
  }
}
