package org.kalypso.model.wspm.core.profil;

import org.kalypso.model.wspm.core.profil.impl.PlainProfil;

/**
 * @author kimwerner
 */
public class ProfilFactory
{
  public static IProfil createProfil( )
  {
    return new PlainProfil();
  }
}