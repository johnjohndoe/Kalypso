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
package org.kalypso.model.wspm.tuhh.core.util;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.wrappers.ProfilePointMarkerWrapper;
import org.kalypso.model.wspm.core.profil.wrappers.ProfilePointWrapper;
import org.kalypso.model.wspm.core.profil.wrappers.ProfileWrapper;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;

/**
 * @author Dirk Kuch
 */
public class WspmProfileHelper extends org.kalypso.model.wspm.core.util.WspmProfileHelper
{

  /**
   * @return breite of sohlpunkt
   */
  public static double findSohlpunkt( final IProfil profile )
  {
    final ProfileWrapper wrapper = new ProfileWrapper( profile );
    final ProfilePointMarkerWrapper[] dbs = wrapper.getProfilePointMarkerWrapper( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
    if( dbs.length != 2 )
      throw new IllegalStateException();

    double sohlpunkt = Double.MAX_VALUE;
    final List<ProfilePointWrapper> sohle = new ArrayList<ProfilePointWrapper>();
    boolean lastIterationAdd = false;

    final ProfilePointWrapper[] points = wrapper.findPointsBetween( dbs[0].getBreite(), dbs[1].getBreite(), true );
    for( final ProfilePointWrapper point : points )
    {
      final Double h = point.getHoehe();

      if( h < sohlpunkt )
      {
        sohle.clear();

        sohlpunkt = h;
        sohle.add( point );

        lastIterationAdd = true;
      }
      else if( h == sohlpunkt && lastIterationAdd == true )
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

    final ProfilePointWrapper p1 = sohle.get( 0 );
    final ProfilePointWrapper p2 = sohle.get( sohle.size() - 1 );

    final double distance = Math.abs( p1.getBreite() - p2.getBreite() );

    return p1.getBreite() + distance / 2.0;
  }
}