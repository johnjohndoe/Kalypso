package com.bce.eind.core.profil;

import java.util.Collection;
import java.util.List;

import com.bce.eind.core.profil.IPlainProfil.BUILDING_TYP;

/**
 * @author kimwerner
 */
public interface IProfilBuilding
{
  public BUILDING_TYP getBuildingTyp( );

  public List<ProfilPointProperty> getProfilPointProperties( );

  public Collection<ProfilBuildingProperty> getProfilBuildingProperties( );

  public double getValue( final ProfilBuildingProperty buildingValue )
      throws ProfilBuildingException;

  public void setValue( final ProfilBuildingProperty buildingValue, final double value )
      throws ProfilBuildingException;

  public int getProfilPointPropertiesCount( );

  public boolean hasProperty( final ProfilBuildingProperty profilBuildingProperty );
}
