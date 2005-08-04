package com.bce.eind.core.profil.impl.buildings;

import java.util.Arrays;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.ProfilBuildingProperty;
import com.bce.eind.core.profil.PointProperty;

/**
 * @author kimwerner
 */
public class BrueckeProfilBuilding extends AbstractProfilBuilding
{
  public BrueckeProfilBuilding( )
  {
    super( IProfil.BUILDING_TYP.BRUECKE, Arrays.asList( ProfilBuildingProperty.BREITE,
        ProfilBuildingProperty.UNTERWASSER, ProfilBuildingProperty.PFEILERFORM,
        ProfilBuildingProperty.RAUHEIT ), new PointProperty[] { PointProperty.UNTERKANTEBRUECKE, PointProperty.OBERKANTEBRUECKE } );
  }
}
