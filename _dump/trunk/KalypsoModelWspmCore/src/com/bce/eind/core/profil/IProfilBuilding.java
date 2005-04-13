/*
 * Created on 31.03.2005
  */
package com.bce.eind.core.profil;

import java.util.List;


import com.bce.eind.core.profil.IProfil.BUILDING_TYP;
import com.bce.eind.core.profil.IProfil.BUILDING_VALUES;

/**
 * @author kimwerner
  */
public interface IProfilBuilding
{
public BUILDING_TYP getBuildingTyp();
public List<IProfilPointProperty> getProfilPointProperties();
public double getValue(BUILDING_VALUES buildingValue) throws ProfilBuildingException;
public void setValue(BUILDING_VALUES buildingValue,double value) throws ProfilBuildingException;
public int getProfilPointPropertiesCount();
}
