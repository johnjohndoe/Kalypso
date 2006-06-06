package org.kalypso.model.wspm.core.profil;

import org.kalypso.model.wspm.core.profil.impl.PlainProfil;
import org.kalypso.model.wspm.core.profil.serializer.IProfilSource;


/**
 * @author kimwerner
 *  
 */
public class ProfilFactory
{
  public static IProfil createProfil(final IProfilSource src)
  {
    return new PlainProfil(src);
}
}