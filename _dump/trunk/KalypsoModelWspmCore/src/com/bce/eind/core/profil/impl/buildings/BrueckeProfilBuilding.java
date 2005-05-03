/*
 * Created on 31.03.2005
 */
package com.bce.eind.core.profil.impl.buildings;

import java.util.Arrays;

import com.bce.eind.core.profil.ProfilBuildingProperty;
import com.bce.eind.core.profil.ProfilPointProperty;
import com.bce.eind.core.profil.IProfil.BUILDING_TYP;

/**
 * @author kimwerner
 */
public class BrueckeProfilBuilding extends AbstractProfilBuilding
{

  public BrueckeProfilBuilding()
  {
    super(BUILDING_TYP.BLD_BRUECKE,Arrays.asList( ProfilBuildingProperty.BREITE, ProfilBuildingProperty.HOEHE,
        ProfilBuildingProperty.STEIGUNG, ProfilBuildingProperty.SOHLGEFAELLE, ProfilBuildingProperty.BEZUGSPUNKT_X,
        ProfilBuildingProperty.BEZUGSPUNKT_Y ));
    m_pointProperties.add(ProfilPointProperty.OBERKANTEBRUECKE);
    m_pointProperties.add(ProfilPointProperty.UNTERKANTEBRUECKE);
  }
 }
