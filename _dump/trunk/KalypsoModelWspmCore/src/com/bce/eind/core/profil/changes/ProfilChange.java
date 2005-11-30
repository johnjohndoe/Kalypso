package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfil.PROFIL_PROPERTY;
import com.bce.eind.core.profil.impl.PlainProfil;

public class ProfilChange extends AbstractChange
{

  public ProfilChange( final IProfil profil, final Object property, final Object newValue )
  {
    super( profil, property, newValue );
  }
  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profil.changes.AbstractChange#doChange(com.bce.eind.core.profil.IProfil)
   */
  @Override
  public EventToFire doChange() throws ProfilDataException
  {
    ((PlainProfil)m_object).setProperty(m_property, m_newValue );
    return EventToFire.PROFIL_CHANGED ;
  }
  /**
   * @see com.bce.eind.core.profil.changes.AbstractChange#getUndoChange()
   */
  @Override
  public AbstractChange getUndoChange( ) throws ProfilDataException
  {
    final IProfil p = (IProfil)m_object;
    final PROFIL_PROPERTY pp = (PROFIL_PROPERTY)m_property;
    final Object oldValue = p.getProperty( pp );
    return new ProfilChange( p, pp, oldValue );
  }
}
