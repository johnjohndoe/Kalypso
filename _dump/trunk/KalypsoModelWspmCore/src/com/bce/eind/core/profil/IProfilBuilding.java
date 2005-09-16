package com.bce.eind.core.profil;

import java.util.Collection;

import com.bce.eind.core.profil.IProfilConstants.BUILDING_PROPERTY;
import com.bce.eind.core.profil.IProfilConstants.BUILDING_TYP;

/**
 * @author kimwerner
 */
public interface IProfilBuilding
{
  public BUILDING_TYP getBuildingTyp( );

  public PointProperty[] getProfilPointProperties( );

  public Collection<BUILDING_PROPERTY> getProfilBuildingProperties( );

  public double getValue( final BUILDING_PROPERTY buildingValue )
      throws ProfilBuildingException;

  /** @return true, if the value was changed, false if bothing happended */
  public boolean setValue( final BUILDING_PROPERTY buildingValue, final double value )
      throws ProfilBuildingException;

  public boolean hasProperty( final BUILDING_PROPERTY profilBuildingProperty );
 
}
