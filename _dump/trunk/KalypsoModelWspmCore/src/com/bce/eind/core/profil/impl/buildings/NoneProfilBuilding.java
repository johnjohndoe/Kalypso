package com.bce.eind.core.profil.impl.buildings;

import java.util.ArrayList;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.ProfilBuildingProperty;

/**
 * @author kimwerner
 */
public class NoneProfilBuilding extends AbstractProfilBuilding
{
  public NoneProfilBuilding( )
  {
    super( IProfil.BUILDING_TYP.NONE, new ArrayList<ProfilBuildingProperty>( 0 ) );
  }
}
