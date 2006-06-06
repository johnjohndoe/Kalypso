/*
 * Created on 31.03.2005
 */
package org.kalypso.model.wspm.core.profil;

import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_TYP;
import org.kalypso.model.wspm.core.profil.impl.devider.ProfilDevider;


/**
 * @author kimwerner
 */
public class ProfilDeviderFactory
{

  public static IProfilDevider createDevider( final DEVIDER_TYP deviderTyp,final IProfilPoint point )
  {
   
        return new ProfilDevider(deviderTyp,point);
  
  }

}
