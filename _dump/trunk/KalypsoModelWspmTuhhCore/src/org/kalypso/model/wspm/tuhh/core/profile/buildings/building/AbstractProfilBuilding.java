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
package org.kalypso.model.wspm.tuhh.core.profile.buildings.building;

import java.util.Collection;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.AbstractBuilding;

/**
 * @author kimwerner
 */
public abstract class AbstractProfilBuilding extends AbstractBuilding
{

  private final String[] m_pointProperties;

  public AbstractProfilBuilding( final String buildingTyp, final String name, final Collection<String> properties, final String[] pointProperties )
  {
    super( buildingTyp, name, properties );

    m_pointProperties = pointProperties == null ? new String[0] : pointProperties;

  }

  @SuppressWarnings("unused")
  public void addProfilProperties( final IProfil profil ) throws ProfilDataException
  {
    for( final String property : getPointProperties() )
      profil.addPointProperty( property );
  }

  @SuppressWarnings("unused")
  public void removeProfilProperties( final IProfil profil ) throws ProfilDataException
  {
    for( final String property : getPointProperties() )
      profil.removePointProperty( property );
  }

  /**
   * @return Returns the buildingTyp.
   */
  @Override
  public String getId( )
  {
    return m_buildingTyp;
  }

  /**
   * @see org.kalypso.model.wspm.core.profilinterface.IProfilBuilding#getTableDataKeys()
   */
  public String[] getPointProperties( )
  {
    return m_pointProperties;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfileObject#hasProperty(org.kalypso.model.wspm.core.profil.BUILDING_PROPERTY)
   */
  @Override
  public boolean hasProperty( String profilBuildingProperty )
  {
    return m_buildingValues.containsKey( profilBuildingProperty );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfileObject#getBuildingProperties()
   */
  @Override
  public String[] getObjectProperties( )
  {
    return m_buildingValues.keySet().toArray( new String[0] );
  }
  /**
   * @see org.kalypso.model.wspm.core.profil.IProfileObject#getLabelFor(java.lang.String)
   */
  public String getLabelFor( String key )
  {
        return key.subSequence(IWspmTuhhConstants.BUILDING_PROPERTY.length(),key.length() ).toString();
  }

}
