/*
 * Created on 31.03.2005
 */
package com.bce.eind.core.profil.impl.buildings;

import java.util.ArrayList;

import com.bce.eind.core.profil.ProfilBuildingProperty;
import com.bce.eind.core.profil.IProfil.BUILDING_TYP;

/**
 * @author kimwerner
 */
public class NoneProfilBuilding extends AbstractProfilBuilding
{

  public NoneProfilBuilding()
  {
    super(BUILDING_TYP.BLD_NONE,new ArrayList<ProfilBuildingProperty>(0));
  }
 }
