package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.IProfilDevider;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilDevider.DEVIDER_TYP;
import com.bce.eind.core.profil.impl.PlainProfil;

public class DeviderAdd implements IProfilChange
{
  private final IProfil m_profil;

  private final DEVIDER_TYP m_typ;

  private final IProfilPoint m_point;

  public DeviderAdd(final IProfil profil, final DEVIDER_TYP typ,final IProfilPoint position)
      
  {
    m_typ = typ;
    m_profil = profil;
    m_point = position;
  }

  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profil.changes.AbstractChange#doChange(PlainProfil)
   */
  public IProfilChange doChange( final ProfilChangeHint hint ) throws ProfilDataException
  {
    hint.setDeviderMove();
    
    final IProfilDevider devider = m_profil.addDevider(m_point,m_typ); 
    return new DeviderRemove(m_profil, devider);
  }

 
}
