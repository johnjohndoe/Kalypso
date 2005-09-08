package com.bce.eind.core.profil.impl.buildings;

import java.util.Arrays;

import com.bce.eind.core.profil.PointProperty;
import com.bce.eind.core.profil.IProfilConstants.BUILDING_PROPERTY;
import com.bce.eind.core.profil.IProfilConstants.BUILDING_TYP;

/**
 * @author kimwerner
 */
public class BrueckeProfilBuilding extends AbstractProfilBuilding
{
  public BrueckeProfilBuilding( )
  {
    super( BUILDING_TYP.BRUECKE, Arrays.asList( BUILDING_PROPERTY.BREITE,
        BUILDING_PROPERTY.UNTERWASSER, BUILDING_PROPERTY.PFEILERFORM,
        BUILDING_PROPERTY.RAUHEIT ), new PointProperty[] { PointProperty.UNTERKANTEBRUECKE, PointProperty.OBERKANTEBRUECKE } );
  }
}
