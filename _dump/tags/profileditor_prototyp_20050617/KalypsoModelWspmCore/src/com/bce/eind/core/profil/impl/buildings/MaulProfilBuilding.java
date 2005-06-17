package com.bce.eind.core.profil.impl.buildings;

import java.util.Arrays;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.ProfilBuildingProperty;

/**
 * @author kimwerner
 */
public class MaulProfilBuilding extends AbstractProfilBuilding
{

  public MaulProfilBuilding( )
  {
    super( IProfil.BUILDING_TYP.MAUL, Arrays.asList( ProfilBuildingProperty.BEZUGSPUNKT_X,
        ProfilBuildingProperty.BEZUGSPUNKT_Y, ProfilBuildingProperty.HOEHE,
        ProfilBuildingProperty.BREITE, ProfilBuildingProperty.SOHLGEFAELLE,
        ProfilBuildingProperty.RAUHEIT ), null );
  }
}
