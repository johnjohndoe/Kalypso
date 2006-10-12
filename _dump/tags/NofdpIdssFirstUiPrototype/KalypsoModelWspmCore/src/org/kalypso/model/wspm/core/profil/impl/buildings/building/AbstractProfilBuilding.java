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
package org.kalypso.model.wspm.core.profil.impl.buildings.building;

import java.util.Collection;
import java.util.Collections;

import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.impl.PlainProfil;
import org.kalypso.model.wspm.core.profil.impl.buildings.AbstractBuilding;


/**
 * @author kimwerner
 */
public abstract class AbstractProfilBuilding extends AbstractBuilding
{

  private final POINT_PROPERTY[] m_pointProperties;

  public AbstractProfilBuilding( final BUILDING_TYP buildingTyp,
      final Collection<BUILDING_PROPERTY> properties, final POINT_PROPERTY[] pointProperties )
  {
    super( buildingTyp, properties );

    m_pointProperties = pointProperties == null ? new POINT_PROPERTY[] {} : pointProperties;

  }

  @SuppressWarnings("unused")
  public void addProfilProperties( final PlainProfil profil ) throws ProfilDataException
  {
    for( final POINT_PROPERTY property : getPointProperties() )
      profil.addPointProperty( property );
  }

  @SuppressWarnings("unused")
  public void removeProfilProperties( final PlainProfil profil ) throws ProfilDataException
  {
    for( final POINT_PROPERTY property : getPointProperties() )
      profil.removePointProperty( property );
  }

  /**
   * @return Returns the buildingTyp.
   */
  @Override
  public BUILDING_TYP getTyp( )
  {
    return m_buildingTyp;
  }

  /**
   * @see org.kalypso.model.wspm.core.profilinterface.IProfilBuilding#getTableDataKeys()
   */
  public POINT_PROPERTY[] getPointProperties( )
  {
    return m_pointProperties;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilBuilding#hasProperty(org.kalypso.model.wspm.core.profil.BUILDING_PROPERTY)
   */
  @Override
  public boolean hasProperty( BUILDING_PROPERTY profilBuildingProperty )
  {
    return m_buildingValues.containsKey( profilBuildingProperty );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilBuilding#getBuildingProperties()
   */
  @Override
  public Collection<BUILDING_PROPERTY> getBuildingProperties( )
  {
    return Collections.unmodifiableCollection( m_buildingValues.keySet() );
  }
}
