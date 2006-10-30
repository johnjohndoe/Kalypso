/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.tuhh.ui.resolutions;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_TYP;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.changes.ActiveObjectEdit;
import org.kalypso.model.wspm.core.profil.changes.DeviderMove;

/**
 * @author kimwerner
 */

public class MoveDeviderResolution extends AbstractProfilMarkerResolution
{
  final private int m_deviderIndex;

  final private DEVIDER_TYP m_deviderTyp;

  /**
   * verschiebt die Trennfl�che auf den linken Trenner "Durchstr�mter Bereich"
   * 
   * @param deviderTyp,deviderIndex
   *          devider=IProfil.getDevider(deviderTyp)[deviderIndex]
   */
  public MoveDeviderResolution( final int deviderIndex, final DEVIDER_TYP deviderTyp )
  {
    super( "verschieben der Trennfl�che in den durchstr�mten Bereich", null, null );
    m_deviderIndex = deviderIndex;
    m_deviderTyp = deviderTyp;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#resolve(org.kalypso.model.wspm.core.profil.IProfil,
   *      org.eclipse.core.resources.IMarker)
   */
  @Override
  protected IProfilChange[] resolve( IProfil profil )
  {
    final IProfilDevider[] deviders1 = profil.getDevider( m_deviderTyp );
    final IProfilDevider devider1 = m_deviderIndex < deviders1.length ? deviders1[m_deviderIndex] : null;
    if( devider1 == null )
      return null;
    final IProfilDevider[] deviders2 = profil.getDevider( DEVIDER_TYP.DURCHSTROEMTE );
    final IProfilDevider devider2 = deviders2[0];
    if( devider2 == null )
      return null;
    return new IProfilChange[] { new DeviderMove( devider1, devider2.getPoint() ), new ActiveObjectEdit( profil, devider2.getPoint(), POINT_PROPERTY.BREITE ) };
  }

}
