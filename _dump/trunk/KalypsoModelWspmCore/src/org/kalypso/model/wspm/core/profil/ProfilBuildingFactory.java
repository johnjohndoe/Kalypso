/*
 * Created on 31.03.2005
 */
package org.kalypso.model.wspm.core.profil;

import org.kalypso.model.wspm.core.profil.IProfilBuilding.BUILDING_TYP;
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

  public static IProfilBuilding createProfilBuilding( final BUILDING_TYP buildingTyp )
  {
    switch( buildingTyp )
    {

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
        return null;
    }

  }

}
