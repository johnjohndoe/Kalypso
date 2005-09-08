package com.bce.eind.core.profil.impl.buildings;

import java.util.Arrays;

import com.bce.eind.core.profil.PointProperty;
import com.bce.eind.core.profil.IProfilConstants.BUILDING_PROPERTY;
import com.bce.eind.core.profil.IProfilConstants.BUILDING_TYP;

public class WehrProfilBuilding extends AbstractProfilBuilding
{

  public WehrProfilBuilding()
  {
    super( BUILDING_TYP.WEHR, Arrays.asList( BUILDING_PROPERTY.WEHRART),  new PointProperty[] { PointProperty.OBERKANTEWEHR} );
  }

}
