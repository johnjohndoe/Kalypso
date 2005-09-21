/*
 * Created on 31.03.2005
 */
package com.bce.eind;

import com.bce.eind.core.profil.IProfilBuilding;
import com.bce.eind.core.profil.IProfilConstants.BUILDING_TYP;
import com.bce.eind.core.profil.impl.buildings.BuildingNone;
import com.bce.eind.core.profil.impl.buildings.building.BuildingBruecke;
import com.bce.eind.core.profil.impl.buildings.building.BuildingWehr;
import com.bce.eind.core.profil.impl.buildings.durchlass.BuildingEi;
import com.bce.eind.core.profil.impl.buildings.durchlass.BuildingKreis;
import com.bce.eind.core.profil.impl.buildings.durchlass.BuildingMaul;
import com.bce.eind.core.profil.impl.buildings.durchlass.BuildingTrapez;

/**
 * @author kimwerner
 */
public class ProfilBuildingFactory
{

  public static IProfilBuilding createProfilBuilding( final BUILDING_TYP buildingTyp )
  {
    switch( buildingTyp )
    {
      case NONE:
        return new BuildingNone();
      case EI:
        return new BuildingEi();
      case KREIS:
        return new BuildingKreis();
      case TRAPEZ:
        return new BuildingTrapez();
      case MAUL:
        return new BuildingMaul();
      case BRUECKE:
        return new BuildingBruecke();
      case WEHR:
        return new BuildingWehr();
      default:
        return new BuildingNone();
    }

  }

}
