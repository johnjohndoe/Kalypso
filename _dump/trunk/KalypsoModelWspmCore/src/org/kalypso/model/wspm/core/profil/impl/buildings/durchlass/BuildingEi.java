package org.kalypso.model.wspm.core.profil.impl.buildings.durchlass;

import java.util.Arrays;

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
