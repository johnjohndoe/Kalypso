/*
 * Created on 31.03.2005
 */
package com.bce.eind.core.profil;

import com.bce.eind.core.profil.IProfilDevider.DEVIDER_TYP;
import com.bce.eind.core.profil.impl.devider.ProfilDevider;

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
