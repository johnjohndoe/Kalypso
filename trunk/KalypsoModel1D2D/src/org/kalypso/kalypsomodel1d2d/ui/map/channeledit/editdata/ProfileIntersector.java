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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.ChannelEditUtil;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.util.DouglasPeuckerHelper;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.core.util.WspmProfileHelper;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * Initial intersection of an IProfil. There are two ways of intersection:<br>
 * - if there are more profile points than the wished number of intersection points, the intersection is done by
 * Douglas-Peucker<br>
 * - if there are not enough profile points, the intersection is done with an equidistant approach.
 *
 * @param profile
 *          input profile to be intersected.
 * @author Gernot Belger
 * @author Thomas Jung
 */
class ProfileIntersector
{
  private final int m_numPoints;

  public ProfileIntersector( final int numPoints )
  {
    m_numPoints = numPoints;
  }

  public IProfile execute( final IProfile originalProfile )
  {
    final IProfileRecord[] originalRecords = originalProfile.getPoints();
    final int numProfPoints = originalRecords.length;

    final IProfile newProfile = ChannelEditUtil.createEmptyProfile( originalProfile );

    if( m_numPoints > numProfPoints )
      equidistantSegmentation( originalProfile, originalRecords, newProfile );
    else
      douglasPeuckerSegmentation( originalRecords, newProfile );

    return newProfile;
  }

  private void equidistantSegmentation( final IProfile originalProfile, final IProfileRecord[] originalRecords, final IProfile newProfile )
  {
    // copy start point
    final IProfileRecord newStartRecord = newProfile.createProfilPoint();

    /* get values */
    final IProfileRecord startRecord = originalRecords[0];
    final IProfileRecord endRecord = originalRecords[originalRecords.length - 1];

    final double startWidth = startRecord.getBreite();
    final double endWidth = endRecord.getBreite();

    newStartRecord.setBreite( startWidth );
    newStartRecord.setHoehe( startRecord.getHoehe() );
    newStartRecord.setRechtswert( startRecord.getRechtswert() );
    newStartRecord.setHochwert( startRecord.getHochwert() );

    newProfile.addPoint( newStartRecord );

    /* do it by equidistant points */
    // keep in mind, that equidistant widths doesn't get equidistant georeferenced lengths!
    final double totalWidth = endWidth - startWidth;
    final double dWidth = totalWidth / (m_numPoints - 1); // equidistant widths

    for( int i = 1; i < m_numPoints - 1; i++ )
    {
      /* get values */
      final double width = startWidth + i * dWidth;
      final double height = WspmProfileHelper.getHeightByWidth( width, originalProfile );
      final GM_Point geoPoint = WspmProfileHelper.getGeoPosition( width, originalProfile );

      /* set values */
      final IProfileRecord newRecord = newProfile.createProfilPoint();

      newRecord.setBreite( width );
      newRecord.setHoehe( height );
      newRecord.setRechtswert( geoPoint.getX() );
      newRecord.setHochwert( geoPoint.getY() );

      newProfile.addPoint( newRecord );
    }

    /* copy end point */

    final IProfileRecord newEndRecord = newProfile.createProfilPoint();

    newEndRecord.setBreite( endWidth );
    newEndRecord.setHoehe( endRecord.getHoehe() );
    newEndRecord.setRechtswert( endRecord.getRechtswert() );
    newEndRecord.setHochwert( endRecord.getHochwert() );

    newProfile.addPoint( newEndRecord );
  }

  /* do it by Douglas-Peucker */
  private void douglasPeuckerSegmentation( final IProfileRecord[] originalRecords, final IProfile newProfile )
  {
    final IProfileRecord[] vipRecords = DouglasPeuckerHelper.findIProfileVIPPoints( originalRecords, m_numPoints );

    /* Build list of vip point in original order */
    final List<IProfileRecord> simplifiedRecords = new ArrayList<>( Arrays.asList( originalRecords ) );
    simplifiedRecords.retainAll( Arrays.asList( vipRecords ) );

    /* copy record into new profile */
    for( final IProfileRecord element : simplifiedRecords )
    {
      final IProfileRecord newRecord = newProfile.createProfilPoint();

      newRecord.setBreite( element.getBreite() );
      newRecord.setHoehe( element.getHoehe() );
      newRecord.setRechtswert( element.getRechtswert() );
      newRecord.setHochwert( element.getHochwert() );

      newProfile.addPoint( newRecord );
    }
  }
}