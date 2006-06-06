/*
 * Created on 31.03.2005
 */
package org.kalypso.model.wspm.core.profil.impl.buildings.durchlass;

import java.util.Collection;

import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.impl.buildings.AbstractBuilding;


/**
 * @author kimwerner
 */
public abstract class AbstractProfilDurchlass extends AbstractBuilding
{

  public AbstractProfilDurchlass( BUILDING_TYP buildingTyp, Collection<BUILDING_PROPERTY> properties )
  {
    super( buildingTyp, properties );
  }

  public POINT_PROPERTY[] getPointProperties( )
  {
    return null;
  }

}
