/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit.overlay;

import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.ChannelEditUtil;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.core.util.WspmProfileHelper;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Gernot Belger
 */
public class ProfileOverlayMovePointOperation
{
  private final IProfil m_originalProfil;

  private final IProfil m_segmentedProfile;

  public ProfileOverlayMovePointOperation( final IProfil originalProfil, final IProfil segmentedProfile )
  {
    m_originalProfil = originalProfil;
    m_segmentedProfile = segmentedProfile;
  }

  public IProfil moveRecord( final IRecord recordToChange, final double destinationWidth )
  {
    // check if width is less than the first profile point
    final double widthFirstProfilePoint = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, m_originalProfil.getFirstPoint() );
    if( destinationWidth < widthFirstProfilePoint )
      return null;

    // check if width is greater than the last profile point
    final double widthLastProfilePoint = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, m_originalProfil.getLastPoint() );
    if( destinationWidth > widthLastProfilePoint )
      return null;

    /* set the initial height to the profile height */
    /* and get the geo coordinates for the moved profile point */
    final double heigth = WspmProfileHelper.getHeightByWidth( destinationWidth, m_originalProfil );
    final GM_Point gmPoint = WspmProfileHelper.getGeoPosition( destinationWidth, m_originalProfil );
    if( gmPoint == null )
      return null;

    /* set the new profile */
    final IProfil newProfile = createNewProfile( recordToChange, destinationWidth, heigth, gmPoint.getX(), gmPoint.getZ() );

    // FIXME: sort new profile by breite... so dragging point over another is not bad
    final IComponent breiteComponent = newProfile.hasPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    if( breiteComponent != null )
      newProfile.getResult().setSortComponents( new IComponent[] { breiteComponent } );

    return newProfile;

    // TODO: not so nice, use the same profile object for both segments, so that changes goes to both segments
    // directly
    // updateProfileForNeighbourSegment();

    // TODO: both adjacent segments must be updated
    // FIXME:
//    final ISegmentData upstreamSegment = activeProfile.getUpSegment();
//    final ISegmentData downstreamSegment = activeProfile.getDownSegment();
//
//    /* check if the first or last intersection point has been moved -> update intersected profile and bank linestrings */
//    final IProfileRecord[] points = profil.getPoints();
//
//    final double oldStartWdith = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, points[0] );
//    final double oldEndWdith = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, points[points.length - 1] );
//
//    checkIntersectionPoints( upstreamSegment, downstreamSegment, oldStartWdith, oldEndWdith );

    // final SegmentData currentSegment = m_data.getSelectedSegment();
    // currentSegment.updateProfileIntersection();

    // FIXME
    // if( upstreamSegment != null )
    // upstreamSegment.updateProfileIntersection();
    // if( downstreamSegment != null )
    // downstreamSegment.updateProfileIntersection();

    // final CreateChannelData.PROF prof = m_data.getCurrentProfile();
    // FIXME
    // m_data.completationCheck();

    // CreateChannelData.PROF prof = m_data.getCurrentProfile();
    // setProfil( currentSegment.getProfUpIntersProfile() );

    // FIXME: again?
//    if( breiteComponent != null )
//      getProfil().getResult().setSortComponents( new IComponent[] { breiteComponent } );
  }

  private IProfil createNewProfile( final IRecord recordToChange, final double newWidth, final double newHeight, final double newRw, final double newHw )
  {
    final IProfil newProfile = ChannelEditUtil.createEmptyProfile( m_segmentedProfile );

    /* copy old points into new profile and change the dragged record */
    for( final IProfileRecord oldRecord : m_segmentedProfile.getPoints() )
    {
      final IProfileRecord newPoint = newProfile.createProfilPoint();

      if( oldRecord == recordToChange )
      {
        newPoint.setBreite( newWidth );
        newPoint.setHoehe( newHeight );
        newPoint.setRechtswert( newRw );
        newPoint.setHochwert( newHw );
      }
      else
      {
        newPoint.setBreite( oldRecord.getBreite() );
        newPoint.setHoehe( oldRecord.getHoehe() );
        newPoint.setRechtswert( oldRecord.getRechtswert() );
        newPoint.setHochwert( oldRecord.getHochwert() );
      }

      newProfile.addPoint( newPoint );
    }

    return newProfile;
  }
}
