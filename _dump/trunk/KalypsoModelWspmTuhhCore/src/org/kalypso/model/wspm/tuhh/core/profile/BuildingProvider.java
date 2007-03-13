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
package org.kalypso.model.wspm.tuhh.core.profile;

import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfileObjectProvider;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingEi;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingKreis;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingMaul;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.durchlass.BuildingTrapez;

/**
 * @author kimwerner
 */
public class BuildingProvider implements IProfileObjectProvider
{
  private static final String[] m_buildingTypes = { IWspmTuhhConstants.BUILDING_TYP_BRUECKE, IWspmTuhhConstants.BUILDING_TYP_KREIS, IWspmTuhhConstants.BUILDING_TYP_EI,
      IWspmTuhhConstants.BUILDING_TYP_MAUL, IWspmTuhhConstants.BUILDING_TYP_TRAPEZ, IWspmTuhhConstants.BUILDING_TYP_WEHR };

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfileObjectProvider#createObject(java.lang.String)
   */
  public IProfileObject createProfileObject( String profileObjectId )
  {

    if( IWspmTuhhConstants.BUILDING_TYP_BRUECKE.equals( profileObjectId ) )
      return new BuildingBruecke();
    if( IWspmTuhhConstants.BUILDING_TYP_WEHR.equals( profileObjectId ) )
      return new BuildingWehr();
    if( IWspmTuhhConstants.BUILDING_TYP_KREIS.equals( profileObjectId ) )
      return new BuildingKreis();
    if( IWspmTuhhConstants.BUILDING_TYP_MAUL.equals( profileObjectId ) )
      return new BuildingMaul();
    if( IWspmTuhhConstants.BUILDING_TYP_EI.equals( profileObjectId ) )
      return new BuildingEi();
    if( IWspmTuhhConstants.BUILDING_TYP_TRAPEZ.equals( profileObjectId ) )
      return new BuildingTrapez();

    return null;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfileObjectProvider#getObjectIds()
   */
  public String[] getProfileObjectIds( )
  {
    return m_buildingTypes;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfileObjectProvider#providesProfileObject(java.lang.String)
   */
  public boolean providesProfileObject( String objectId )
  {
    for( String building : m_buildingTypes )
    {
      if( building.equals( objectId ) )
        return true;
    }
    return false;
  }

}
