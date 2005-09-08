/*
 * Created on 31.03.2005
 */
package com.bce.eind;

import com.bce.eind.core.profil.IProfilBuilding;
import com.bce.eind.core.profil.IProfilConstants.BUILDING_TYP;
import com.bce.eind.core.profil.impl.buildings.BrueckeProfilBuilding;
import com.bce.eind.core.profil.impl.buildings.EiProfilBuilding;
import com.bce.eind.core.profil.impl.buildings.KreisProfilBuilding;
import com.bce.eind.core.profil.impl.buildings.MaulProfilBuilding;
import com.bce.eind.core.profil.impl.buildings.NoneProfilBuilding;
import com.bce.eind.core.profil.impl.buildings.TrapezProfilBuilding;

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
        return new NoneProfilBuilding();
      case EI:
        return new EiProfilBuilding();
      case KREIS:
        return new KreisProfilBuilding();
      case TRAPEZ:
        return new TrapezProfilBuilding();
      case MAUL:
        return new MaulProfilBuilding();
      case BRUECKE:
        return new BrueckeProfilBuilding();
      default:
        return new NoneProfilBuilding();
    }

  }

}
