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
package org.kalypso.model.wspm.tuhh.ui.resolutions;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.changes.PointMarkerEdit;
import org.kalypso.model.wspm.core.profil.util.ProfilObsHelper;
import org.kalypso.model.wspm.tuhh.core.profile.ProfilDevider;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

/**
 * @author kimwerner
 */

public class AddDeviderResolution extends AbstractProfilMarkerResolution
{
  final private String m_deviderType;

  /**
   * erzeugt fehlende Trennfl‰che
   */
  public AddDeviderResolution( final String deviderType )
  {
    super( "fehlende Trennfl‰chen erzeugen", null, null );
    m_deviderType = deviderType;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#resolve(org.kalypso.model.wspm.core.profil.IProfil,
   *      org.eclipse.core.resources.IMarker)
   */
  @Override
  protected IProfilChange[] resolve( final IProfil profil )
  {
    if( m_deviderType == null )
      return null;
    final IProfilChange[] changes = new IProfilChange[2];
    final IRecord firstP = profil.getPoints().getFirst();
    final IRecord lastP = profil.getPoints().getLast();

    final IComponent component = ProfilObsHelper.getPropertyFromId( profil, m_deviderType );

    changes[0] = new PointMarkerEdit( new ProfilDevider( component, firstP ), true );
    changes[1] = new PointMarkerEdit( new ProfilDevider( component, lastP ), true );
    return changes;
  }

}
