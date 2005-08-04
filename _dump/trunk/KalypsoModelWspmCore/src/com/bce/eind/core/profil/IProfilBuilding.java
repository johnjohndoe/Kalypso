package com.bce.eind.core.profil;

import java.util.Collection;

import com.bce.eind.core.profil.IPlainProfil.BUILDING_TYP;

/**
 * @author kimwerner
 */
public interface IProfilBuilding
{
  public BUILDING_TYP getBuildingTyp( );

  public PointProperty[] getProfilPointProperties( );

  public Collection<ProfilBuildingProperty> getProfilBuildingProperties( );

  public double getValue( final ProfilBuildingProperty buildingValue )
      throws ProfilBuildingException;

  /** @return true, if the value was changed, false if bothing happended */
  public boolean setValue( final ProfilBuildingProperty buildingValue, final double value )
      throws ProfilBuildingException;

  public boolean hasProperty( final ProfilBuildingProperty profilBuildingProperty );
}
