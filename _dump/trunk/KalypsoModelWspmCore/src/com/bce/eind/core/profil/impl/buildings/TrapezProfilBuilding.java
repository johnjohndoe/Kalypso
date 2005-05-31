/*
 * Created on 31.03.2005
 */
package com.bce.eind.core.profil.impl.buildings;

import java.util.Arrays;

import com.bce.eind.core.profil.ProfilBuildingProperty;
import com.bce.eind.core.profil.IProfil.BUILDING_TYP;

/**
 * @author kimwerner
 */
public class TrapezProfilBuilding extends AbstractProfilBuilding
{

  public TrapezProfilBuilding( )
  {
    super( BUILDING_TYP.TRAPEZ, Arrays.asList( ProfilBuildingProperty.BEZUGSPUNKT_X, ProfilBuildingProperty.BEZUGSPUNKT_Y,ProfilBuildingProperty.BREITE, ProfilBuildingProperty.HOEHE,
        ProfilBuildingProperty.STEIGUNG, ProfilBuildingProperty.SOHLGEFAELLE, 
       ProfilBuildingProperty.RAUHEIT ) );
  }
}
