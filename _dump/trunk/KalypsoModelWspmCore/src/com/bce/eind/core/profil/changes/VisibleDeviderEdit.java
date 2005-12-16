package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.IProfilDevider.DEVIDER_TYP;

public class VisibleDeviderEdit implements IProfilChange
{
   private final DEVIDER_TYP m_typ;

  private final boolean m_visible;

  private final IProfil m_profil;
  
  public VisibleDeviderEdit(IProfil profil,final DEVIDER_TYP typ,final boolean isVisible)
   {
    m_typ= typ;
    m_visible = isVisible;
    m_profil = profil;
  }

  /**
   * @see com.bce.eind.core.profil.IProfilChange#doChange(ProfilChangeHint)
   */
  public IProfilChange doChange( final ProfilChangeHint hint)
  {
    hint.setProfilPropertyChanged(true);
    
    final boolean oldVisible = m_profil.getDeviderVisibility(m_typ );
    m_profil.setDeviderVisibility(m_typ, m_visible );
    
    return new VisibleDeviderEdit(m_profil,m_typ,oldVisible );
  }

  
}
