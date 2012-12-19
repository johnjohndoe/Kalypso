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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.util.ProfileUtil;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.core.util.WspmProfileHelper;
import org.kalypso.transformation.transformer.GeoTransformerException;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Gernot Belger
 */
class ProfileOverlayMovePointOperation
{
  private final IProfile m_originalProfil;

  private final IProfile m_workingProfile;

  public ProfileOverlayMovePointOperation( final IProfile originalProfil, final IProfile workingProfile )
  {
    m_originalProfil = originalProfil;
    m_workingProfile = workingProfile;
  }

  public IProfile moveRecord( final IProfileRecord recordToChange, final double destinationWidth ) throws GeoTransformerException
  {
    // check if width is less than the first profile point
    final double widthFirstProfilePoint = ProfileUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, m_originalProfil.getFirstPoint() );
    if( destinationWidth < widthFirstProfilePoint )
      return null;

    // check if width is greater than the last profile point
    final double widthLastProfilePoint = ProfileUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, m_originalProfil.getLastPoint() );
    if( destinationWidth > widthLastProfilePoint )
      return null;

    /* set the initial height to the profile height */
    /* and get the geo coordinates for the moved profile point */
    final double height = WspmProfileHelper.getHeightByWidth( destinationWidth, m_originalProfil );
    final GM_Point gmPoint = WspmProfileHelper.getGeoPositionKalypso( destinationWidth, m_originalProfil );
    if( gmPoint == null )
      return null;

    return createNewProfile( recordToChange, destinationWidth, height, gmPoint.getX(), gmPoint.getY() );
  }

  private IProfile createNewProfile( final IProfileRecord recordToChange, final double newWidth, final double newHeight, final double newRw, final double newHw )
  {
    final IProfile newProfile = ChannelEditUtil.createEmptyProfile( m_workingProfile );

    final List<IProfileRecord> newRecords = new ArrayList<>();

    /* copy old points into new profile and change the dragged record */
    for( final IProfileRecord oldRecord : m_workingProfile.getPoints() )
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

      newRecords.add( newPoint );
    }

    /* sort by width */
    final Comparator<IProfileRecord> widthComparator = new BreiteComparator();
    Collections.sort( newRecords, widthComparator );

    /* add to new profile */
    for( final IProfileRecord record : newRecords )
      newProfile.addPoint( record );

    return newProfile;
  }
}