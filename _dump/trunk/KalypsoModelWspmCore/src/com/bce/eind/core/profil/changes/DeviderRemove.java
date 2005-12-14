package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.IProfilDevider;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.impl.PlainProfil;

public class DeviderRemove implements IProfilChange
{
  private final IProfil m_profil;

  private final IProfilDevider m_devider;


  public DeviderRemove(final IProfil profil, final IProfilDevider devider)
      
  {
    m_devider = devider;
    m_profil = profil;
  }

  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profil.changes.AbstractChange#doChange(PlainProfil)
   */
  public IProfilChange doChange( final ProfilChangeHint hint ) throws ProfilDataException
  {
    hint.setDeviderMoved();
    
    m_profil.removeDevider(m_devider);
    return new DeviderAdd(m_profil, m_devider);
  }

 
}
