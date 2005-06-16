package com.bce.eind.core.profil.impl.buildings;

import java.util.Arrays;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.ProfilBuildingProperty;

/**
 * @author kimwerner
 */
public class EiProfilBuilding extends AbstractProfilBuilding
{
  public EiProfilBuilding( )
  {
    super( IProfil.BUILDING_TYP.EI, Arrays.asList( ProfilBuildingProperty.BREITE,
        ProfilBuildingProperty.HOEHE, ProfilBuildingProperty.SOHLGEFAELLE,
        ProfilBuildingProperty.BEZUGSPUNKT_X, ProfilBuildingProperty.BEZUGSPUNKT_Y,
        ProfilBuildingProperty.RAUHEIT ) );
  }
}
