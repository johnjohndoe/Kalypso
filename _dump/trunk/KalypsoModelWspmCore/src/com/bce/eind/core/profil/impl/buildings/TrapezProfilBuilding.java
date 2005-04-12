/*
 * Created on 31.03.2005
 */
package com.bce.eind.core.profil.impl.buildings;

import java.util.Arrays;

import com.bce.eind.core.profil.IProfil.BUILDING_TYP;
import com.bce.eind.core.profil.IProfil.BUILDING_VALUES;

/**
 * @author kimwerner
 */
public class TrapezProfilBuilding extends AbstractProfilBuilding
{

  public TrapezProfilBuilding()
  {
    super(BUILDING_TYP.BLD_TRAPEZ,Arrays.asList(BUILDING_VALUES.CENTER_X,BUILDING_VALUES.CENTER_Y));
  }
 }
