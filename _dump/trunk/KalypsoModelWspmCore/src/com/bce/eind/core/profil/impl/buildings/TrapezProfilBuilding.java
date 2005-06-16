package com.bce.eind.core.profil.impl.buildings;

import java.util.Arrays;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.ProfilBuildingProperty;

/**
 * @author kimwerner
 */
public class TrapezProfilBuilding extends AbstractProfilBuilding
{
  public TrapezProfilBuilding( )
  {
    super( IProfil.BUILDING_TYP.TRAPEZ, Arrays.asList( ProfilBuildingProperty.BEZUGSPUNKT_X,
        ProfilBuildingProperty.BEZUGSPUNKT_Y, ProfilBuildingProperty.BREITE,
        ProfilBuildingProperty.HOEHE, ProfilBuildingProperty.STEIGUNG,
        ProfilBuildingProperty.SOHLGEFAELLE, ProfilBuildingProperty.RAUHEIT ) );
  }
}
