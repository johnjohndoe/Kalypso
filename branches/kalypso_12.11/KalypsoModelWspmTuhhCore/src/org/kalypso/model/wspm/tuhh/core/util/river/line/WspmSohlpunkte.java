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
package org.kalypso.model.wspm.tuhh.core.util.river.line;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.ArrayUtils;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.core.profil.visitors.ProfileVisitors;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.core.profil.wrappers.ProfileWrapper;
import org.kalypso.model.wspm.core.profil.wrappers.Profiles;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.IProfileBuilding;

/**
 * @author Dirk Kuch
 */
public final class WspmSohlpunkte
{
  private WspmSohlpunkte( )
  {
  }

  public static double findSohlpunkt( final IProfile profile )
  {
    return findSohlpunkt( profile, Profiles.FUZZINESS );
  }

  /**
   * @param fuziness
   *          = height delta -> points inherit (interval!) this delta are equal!
   * @return breite of sohlpunkt
   */
  public static double findSohlpunkt( final IProfile profile, final double fuziness )
  {
    final IProfileRecord startPoint;
    final IProfileRecord endPoint;

    final IProfilePointMarker[] dbs = profile.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
    if( dbs.length != 2 )
    {
      startPoint = profile.getFirstPoint();
      endPoint = profile.getLastPoint();
    }
    else
    {
      startPoint = dbs[0].getPoint();
      endPoint = dbs[1].getPoint();
    }

    final IProfileRecord[] points = ProfileVisitors.findPointsBetween( profile, startPoint.getBreite(), endPoint.getBreite(), true );

    final List<IProfileRecord> sohle = new ArrayList<>();
    boolean lastIterationAdd = false;
    double sohlpunkt = Double.MAX_VALUE;
    for( final IProfileRecord point : points )
    {
      final Double h = point.getHoehe();

      if( h < sohlpunkt - fuziness )
      {
        sohle.clear();

        sohlpunkt = h;
        sohle.add( point );

        lastIterationAdd = true;
      }
      else if( h - sohlpunkt < fuziness && lastIterationAdd )
      {
        sohle.add( point );
      }
      else
      {
        lastIterationAdd = false;
      }
    }

    if( sohle.size() == 1 )
      return sohle.get( 0 ).getBreite();

    final IProfileRecord p1 = sohle.get( 0 );
    final IProfileRecord p2 = sohle.get( sohle.size() - 1 );

    final double distance = Math.abs( p1.getBreite() - p2.getBreite() );

    return p1.getBreite() + distance / 2.0;
  }

  // FIXME: does not belong here or rename "WspmSohlpunkte"
  public static <T extends IProfileBuilding> T getBuilding( final IProfile profile, final Class<T> buildingType )
  {
    final IProfileBuilding[] profileObjects = profile.getProfileObjects( buildingType );
    return ArrayUtils.isEmpty( profileObjects ) ? null : buildingType.cast( profileObjects[0] );
  }

  public static IProfileRecord getSohlpunktPoint( final ProfileWrapper wrapper )
  {
    final IProfilePointMarker[] dbs = wrapper.getProfilePointMarkerWrapper( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
    if( dbs.length != 2 )
      throw new IllegalStateException();

    final IProfileRecord[] points = ProfileVisitors.findPointsBetween( wrapper.getProfile(), dbs[0].getPoint().getBreite(), dbs[1].getPoint().getBreite(), true );
    IProfileRecord ptr = null;

    for( final IProfileRecord point : points )
    {
      if( ptr == null )
        ptr = point;
      else if( point.getHoehe() < ptr.getHoehe() )
        ptr = point;
    }

    return ptr;
  }
}