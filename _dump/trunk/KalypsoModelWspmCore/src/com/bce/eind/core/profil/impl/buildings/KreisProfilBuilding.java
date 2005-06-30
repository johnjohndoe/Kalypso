package com.bce.eind.core.profil.impl.buildings;

import java.util.Arrays;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.ProfilBuildingProperty;

/**
 * @author kimwerner
 */
public class KreisProfilBuilding extends AbstractProfilBuilding
{
  public KreisProfilBuilding( )
  {
    super( IProfil.BUILDING_TYP.KREIS, Arrays.asList( ProfilBuildingProperty.BREITE,
        ProfilBuildingProperty.SOHLGEFAELLE, ProfilBuildingProperty.BEZUGSPUNKT_X,
        ProfilBuildingProperty.BEZUGSPUNKT_Y, ProfilBuildingProperty.RAUHEIT ) );
  }
}
