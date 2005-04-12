package com.bce.eind;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.impl.Profil;

/**
 * @author kimwerner
 *  
 */
public class ProfilFactory
{
  public static IProfil createProfil( )
  {
    return new Profil();
  }
 
}