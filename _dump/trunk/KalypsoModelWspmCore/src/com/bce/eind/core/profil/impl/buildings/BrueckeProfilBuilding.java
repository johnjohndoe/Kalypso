package com.bce.eind.core.profil.impl.buildings;

import java.util.Arrays;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.ProfilBuildingProperty;
import com.bce.eind.core.profil.ProfilPointProperty;

/**
 * @author kimwerner
 */
public class BrueckeProfilBuilding extends AbstractProfilBuilding
{
  public BrueckeProfilBuilding( )
  {
    super( IProfil.BUILDING_TYP.BRUECKE, Arrays.asList( ProfilBuildingProperty.BREITE,
        ProfilBuildingProperty.UNTERWASSER, ProfilBuildingProperty.PFEILERFORM,
        ProfilBuildingProperty.RAUHEIT ) );
    m_pointProperties.add( ProfilPointProperty.OBERKANTEBRUECKE );
    m_pointProperties.add( ProfilPointProperty.UNTERKANTEBRUECKE );
  }
}
