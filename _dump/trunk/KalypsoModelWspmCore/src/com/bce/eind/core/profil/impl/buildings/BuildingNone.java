package com.bce.eind.core.profil.impl.buildings;

import java.util.ArrayList;

import com.bce.eind.core.profil.PointProperty;
import com.bce.eind.core.profil.IProfilConstants.BUILDING_PROPERTY;
import com.bce.eind.core.profil.IProfilConstants.BUILDING_TYP;

/**
 * @author kimwerner
 */
public class BuildingNone extends AbstractBuilding
{
  public BuildingNone( )
  {
    super( BUILDING_TYP.NONE, new ArrayList<BUILDING_PROPERTY>( 0 ));
  }

  public PointProperty[] getProfilPointProperties( )
  {
        return null;
  }

  
}
