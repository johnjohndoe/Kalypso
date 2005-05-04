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
public class MaulProfilBuilding extends AbstractProfilBuilding
{

  public MaulProfilBuilding()
  {
    super(BUILDING_TYP.BLD_MAUL,Arrays.asList( ProfilBuildingProperty.BREITE, ProfilBuildingProperty.HOEHE,
        ProfilBuildingProperty.SOHLGEFAELLE, ProfilBuildingProperty.BEZUGSPUNKT_X,
        ProfilBuildingProperty.BEZUGSPUNKT_Y,ProfilBuildingProperty.RAUHEIT ));
  }
 }
