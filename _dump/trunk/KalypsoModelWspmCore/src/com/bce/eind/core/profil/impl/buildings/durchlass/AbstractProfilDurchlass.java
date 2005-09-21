/*
 * Created on 31.03.2005
 */
package com.bce.eind.core.profil.impl.buildings.durchlass;

import java.util.Collection;

import com.bce.eind.core.profil.PointProperty;
import com.bce.eind.core.profil.IProfilConstants.BUILDING_PROPERTY;
import com.bce.eind.core.profil.IProfilConstants.BUILDING_TYP;
import com.bce.eind.core.profil.impl.buildings.AbstractBuilding;

/**
 * @author kimwerner
 */
public abstract class AbstractProfilDurchlass extends AbstractBuilding
{

  public AbstractProfilDurchlass( BUILDING_TYP buildingTyp, Collection<BUILDING_PROPERTY> properties )
  {
    super( buildingTyp, properties );
  }

  public PointProperty[] getProfilPointProperties( )
  {
    return null;
  }

}
