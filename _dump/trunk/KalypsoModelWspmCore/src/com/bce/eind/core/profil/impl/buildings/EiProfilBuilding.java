package com.bce.eind.core.profil.impl.buildings;

import java.util.Arrays;

import com.bce.eind.core.profil.IProfilConstants.BUILDING_PROPERTY;
import com.bce.eind.core.profil.IProfilConstants.BUILDING_TYP;

/**
 * @author kimwerner
 */
public class EiProfilBuilding extends AbstractProfilBuilding
{
  public EiProfilBuilding( )
  {
    super(BUILDING_TYP.EI, Arrays.asList( BUILDING_PROPERTY.BEZUGSPUNKT_X,
        BUILDING_PROPERTY.BEZUGSPUNKT_Y, BUILDING_PROPERTY.HOEHE,
        BUILDING_PROPERTY.BREITE, BUILDING_PROPERTY.SOHLGEFAELLE,
        BUILDING_PROPERTY.RAUHEIT ), null );
  }
}
