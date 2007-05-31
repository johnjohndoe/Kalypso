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
package org.kalypso.model.wspm.core.profil.changes;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfileObjectProvider;

public class ProfileObjectSet implements IProfilChange
{
  private final IProfil m_profil;

  private final IProfileObject m_object;

  private final String m_objectId;

  /**
   * @param profil
   * @param building
   *          maybe null to remove building
   */
  public ProfileObjectSet( final IProfil profil, final IProfileObject profileObject )
  {
    m_profil = profil;
    m_object = profileObject;
    m_objectId = "null";
  }

  public ProfileObjectSet( final IProfil profil, final String objectId )
  {
    m_profil = profil;
    m_object = null;
    m_objectId = objectId;
  }

  public IProfilChange doChange( final ProfilChangeHint hint )
  {
    if( hint != null )
      hint.setObjectChanged();
    if( hint != null )
      hint.setPointPropertiesChanged();

    final IProfileObject oldObject = m_profil.getProfileObject();
    if( m_object != null  )
      m_profil.setProfileObject( m_object );
    else if ( m_objectId == "null")
      m_profil.removeProfileObject();
    else
    {
      final IProfileObjectProvider pop = m_profil.getObjectProviderFor( m_objectId );
      final IProfileObject object = pop == null ? null : pop.createProfileObject( m_objectId );
      if( object == null )
        return new IllegalChange( m_objectId + " wird nicht unterstützt.",this );
      m_profil.setProfileObject( object );
    }
    return new ProfileObjectSet( m_profil, oldObject );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getObject()
   */
  public Object[] getObjects( )
  {
    return new Object[]{m_object};
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getPointProperty()
   */
  public String getInfo( )
  {

    return m_objectId;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getValue()
   */
  public Double getValue( )
  {

    return null;
  }

}
