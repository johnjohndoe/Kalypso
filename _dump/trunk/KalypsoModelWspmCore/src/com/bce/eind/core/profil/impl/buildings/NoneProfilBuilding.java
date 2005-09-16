package com.bce.eind.core.profil.impl.buildings;

import java.util.ArrayList;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilConstants.BUILDING_PROPERTY;
import com.bce.eind.core.profil.IProfilConstants.BUILDING_TYP;

/**
 * @author kimwerner
 */
public class NoneProfilBuilding extends AbstractProfilBuilding
{
  public NoneProfilBuilding( )
  {
    super( BUILDING_TYP.NONE, new ArrayList<BUILDING_PROPERTY>( 0 ), null );
  }

  
}
