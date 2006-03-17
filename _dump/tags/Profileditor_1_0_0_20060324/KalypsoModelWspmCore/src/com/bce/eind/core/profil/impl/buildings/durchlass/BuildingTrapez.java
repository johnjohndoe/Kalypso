package com.bce.eind.core.profil.impl.buildings.durchlass;

import java.util.Arrays;

/**
 * @author kimwerner
 */
public class BuildingTrapez extends AbstractProfilDurchlass
{
  public BuildingTrapez( )
  {
    super( BUILDING_TYP.TRAPEZ, Arrays.asList( BUILDING_PROPERTY.BEZUGSPUNKT_X,
        BUILDING_PROPERTY.BEZUGSPUNKT_Y, BUILDING_PROPERTY.BREITE,
        BUILDING_PROPERTY.HOEHE, BUILDING_PROPERTY.STEIGUNG,
        BUILDING_PROPERTY.SOHLGEFAELLE, BUILDING_PROPERTY.RAUHEIT ));
  }

 
}
