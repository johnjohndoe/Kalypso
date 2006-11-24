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
package org.kalypso.model.wspm.core.profil;

import org.kalypso.model.wspm.core.profil.impl.buildings.building.BuildingBruecke;
import org.kalypso.model.wspm.core.profil.impl.buildings.building.BuildingWehr;
import org.kalypso.model.wspm.core.profil.impl.buildings.durchlass.BuildingEi;
import org.kalypso.model.wspm.core.profil.impl.buildings.durchlass.BuildingKreis;
import org.kalypso.model.wspm.core.profil.impl.buildings.durchlass.BuildingMaul;
import org.kalypso.model.wspm.core.profil.impl.buildings.durchlass.BuildingTrapez;

/**
 * @author kimwerner
 */
public class ProfilBuildingFactory
{
  public static IProfilBuilding createProfilBuilding( final String buildingTyp )
  {
    if( buildingTyp.compareTo( IProfilConstants.BUILDING_TYP_EI ) == 0 )
      return new BuildingEi();
    else if( buildingTyp.compareTo( IProfilConstants.BUILDING_TYP_KREIS ) == 0 )
      return new BuildingKreis();
    else if( buildingTyp.compareTo( IProfilConstants.BUILDING_TYP_TRAPEZ ) == 0 )
      return new BuildingTrapez();
    else if( buildingTyp.compareTo( IProfilConstants.BUILDING_TYP_MAUL ) == 0 )
      return new BuildingMaul();
    else if( buildingTyp.compareTo( IProfilConstants.BUILDING_TYP_BRUECKE ) == 0 )
      return new BuildingBruecke();
    else if( buildingTyp.compareTo( IProfilConstants.BUILDING_TYP_WEHR ) == 0 )
      return new BuildingWehr();
    else
      return null;
  }
}
