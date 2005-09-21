package com.bce.eind.core.profil.impl.buildings.durchlass;

import java.util.Arrays;

import com.bce.eind.core.profil.IProfilConstants.BUILDING_PROPERTY;
import com.bce.eind.core.profil.IProfilConstants.BUILDING_TYP;

/**
 * @author kimwerner
 */
public class BuildingKreis extends AbstractProfilDurchlass
{
  public BuildingKreis( )
  {
    super( BUILDING_TYP.KREIS, Arrays.asList( BUILDING_PROPERTY.BEZUGSPUNKT_X,
        BUILDING_PROPERTY.BEZUGSPUNKT_Y, BUILDING_PROPERTY.BREITE, BUILDING_PROPERTY.SOHLGEFAELLE,
        BUILDING_PROPERTY.RAUHEIT ) );
  }

}
