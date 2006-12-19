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
package org.kalypso.model.wspm.tuhh.core.profile;

import java.util.LinkedList;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilConstants;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.filter.AbstractProfilePointFilter;
import org.kalypso.model.wspm.core.profil.filter.IProfilePointFilter;

/**
 * @author Gernot Belger
 */
public class VorlandProfilePointFilter extends AbstractProfilePointFilter implements IProfilePointFilter
{
  /**
   * @see org.kalypso.model.wspm.core.profil.filter.IProfilePointFilter#accept(org.kalypso.model.wspm.core.profil.IProfil,
   *      org.kalypso.model.wspm.core.profil.IProfilPoint)
   */
  public boolean accept( final IProfil profil, final IProfilPoint point )
  {
    final IProfilDevider[] devider = profil.getDevider( IProfilConstants.DEVIDER_TYP_TRENNFLAECHE );
    if( devider.length != 2 )
      return true;

    final IProfilPoint leftPoint = devider[0].getPoint();
    final IProfilPoint rightPoint = devider[1].getPoint();

    final LinkedList<IProfilPoint> points = profil.getPoints();

    final int left = points.indexOf( leftPoint );
    final int right = points.indexOf( rightPoint );
    final int index = points.indexOf( point );

    return !(left <= index && index < right);
  }

}
