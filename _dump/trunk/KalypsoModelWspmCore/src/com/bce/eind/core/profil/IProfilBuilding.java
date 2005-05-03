/*
 * Created on 31.03.2005
  */
package com.bce.eind.core.profil;

import java.util.List;

import com.bce.eind.core.profil.IProfil.BUILDING_TYP;

/**
 * @author kimwerner
  */
public interface IProfilBuilding
{
public BUILDING_TYP getBuildingTyp();
public List<ProfilPointProperty> getProfilPointProperties();
public double getValue(ProfilBuildingProperty buildingValue) throws ProfilBuildingException;
public void setValue(ProfilBuildingProperty buildingValue,double value) throws ProfilBuildingException;
public int getProfilPointPropertiesCount();
}
