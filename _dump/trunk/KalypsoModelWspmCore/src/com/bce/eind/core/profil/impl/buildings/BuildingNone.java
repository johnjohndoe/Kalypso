package com.bce.eind.core.profil.impl.buildings;

import java.util.ArrayList;

import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;

/**
 * @author kimwerner
 */
public class BuildingNone extends AbstractBuilding
{
  public BuildingNone( )
  {
    super( BUILDING_TYP.NONE, new ArrayList<BUILDING_PROPERTY>( 0 ));
  }

  public POINT_PROPERTY[] getPointProperties( )
  {
        return null;
  }

  
}
