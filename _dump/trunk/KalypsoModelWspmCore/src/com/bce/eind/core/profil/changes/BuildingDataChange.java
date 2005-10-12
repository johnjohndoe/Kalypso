package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfilBuilding;
import com.bce.eind.core.profil.IProfilBuilding.BUILDING_PROPERTY;


public class BuildingDataChange
{

  private final IProfilBuilding m_building;

  private final BUILDING_PROPERTY m_property;

  private final Object m_newValue;

  public BuildingDataChange( final IProfilBuilding building, final BUILDING_PROPERTY property, final Object newValue )
  {
    m_building = building;
    m_property = property;
    m_newValue = newValue;
  }
}
