package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfilListener;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.impl.PlainProfil;

public class ProfilChange extends AbstractChange
{

  /**
   * @see com.bce.eind.core.profil.changes.AbstractChange#fireEvent(com.bce.eind.core.profil.IProfilListener)
   */
  @Override
  public void fireEvent( IProfilListener listener )
  {
    listener.onProfilDataChanged(this);
    
  }
  public ProfilChange(final Object property, final Object newValue )
  {
    super( null, property, newValue );
  }
  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profil.changes.AbstractChange#doChange(PlainProfil)
   */
  @Override
  public void doChange(PlainProfil profil) throws ProfilDataException
  {
    m_oldValue = profil.getProperty(m_property);
    profil.setProperty(m_property, m_newValue );

  }
  /**
   * @see com.bce.eind.core.profil.changes.AbstractChange#getUndoChange()
   */
  @Override
  public AbstractChange getUndoChange( ) throws ProfilDataException
  {
        return new ProfilChange(m_property, m_oldValue );
  }
}
