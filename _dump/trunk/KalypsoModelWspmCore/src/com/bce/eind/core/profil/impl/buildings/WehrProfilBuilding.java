package com.bce.eind.core.profil.impl.buildings;

import java.util.Arrays;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.ProfilBuildingProperty;
import com.bce.eind.core.profil.ProfilPointProperty;

public class WehrProfilBuilding extends AbstractProfilBuilding
{

  public WehrProfilBuilding()
  {
    super( IProfil.BUILDING_TYP.WEHR, Arrays.asList( ProfilBuildingProperty.WEHRART),  new ProfilPointProperty[] { ProfilPointProperty.OBERKANTEWEHR} );
  }

}
