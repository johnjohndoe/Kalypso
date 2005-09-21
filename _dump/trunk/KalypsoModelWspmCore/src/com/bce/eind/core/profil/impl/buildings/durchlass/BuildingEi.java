package com.bce.eind.core.profil.impl.buildings.durchlass;

import java.util.Arrays;

import com.bce.eind.core.profil.IProfilConstants.BUILDING_PROPERTY;
import com.bce.eind.core.profil.IProfilConstants.BUILDING_TYP;

/**
 * @author kimwerner
 */
public class BuildingEi extends AbstractProfilDurchlass
{
  public BuildingEi( )
  {
    super(BUILDING_TYP.EI, Arrays.asList( BUILDING_PROPERTY.BEZUGSPUNKT_X,
        BUILDING_PROPERTY.BEZUGSPUNKT_Y, BUILDING_PROPERTY.HOEHE,
        BUILDING_PROPERTY.BREITE, BUILDING_PROPERTY.SOHLGEFAELLE,
        BUILDING_PROPERTY.RAUHEIT ) );
  }

 
}
