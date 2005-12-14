package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.impl.PlainProfil;

public final class ProfilPropertyEdit implements IProfilChange
{
  private IProfil m_profil;
  private final Object m_property;

  private final Object m_newValue;

  public ProfilPropertyEdit( final IProfil profil, final Object property, final Object newValue )
  {
    m_profil = profil;
    m_property = property;
    m_newValue = newValue;
  }

  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profil.changes.AbstractChange#doChange(PlainProfil)
   */
  public IProfilChange doChange( final ProfilChangeHint hint ) throws ProfilDataException
  {
    hint.setProfilPropertyChanged(true);
    
    final Object oldValue = m_profil.getProperty(m_property);
    m_profil.setProperty( m_property, m_newValue );

    return new ProfilPropertyEdit( m_profil, m_property, oldValue );
  }

 
}