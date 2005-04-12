/*
 * Created on 31.03.2005
 */
package com.bce.eind;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilBuilding;
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

  public static IProfilBuilding createProfilBuilding( final IProfil.BUILDING_TYP buildingTyp )
  {
    switch( buildingTyp )
    {
      case BLD_NONE:
        return new NoneProfilBuilding();
      case BLD_EI:
        return new EiProfilBuilding();
      case BLD_KREIS:
        return new KreisProfilBuilding();
      case BLD_TRAPEZ:
        return new TrapezProfilBuilding();
      case BLD_MAUL:
        return new MaulProfilBuilding();
      case BLD_BRUECKE:
        return new BrueckeProfilBuilding();
      default:
        return new NoneProfilBuilding();
    }

  }

}
