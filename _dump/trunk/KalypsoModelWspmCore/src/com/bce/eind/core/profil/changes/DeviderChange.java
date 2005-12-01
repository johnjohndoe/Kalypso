package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfilDevider;
import com.bce.eind.core.profil.IProfilListener;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilDevider.DEVIDER_PROPERTY;
import com.bce.eind.core.profil.impl.PlainProfil;
import com.bce.eind.core.profil.impl.devider.ProfilDevider;

public class DeviderChange extends AbstractChange
{

  /**
   * @see com.bce.eind.core.profil.changes.AbstractChange#fireEvent(com.bce.eind.core.profil.IProfilListener)
   */
  @Override
  public void fireEvent( IProfilListener listener )
  {
    listener.onDeviderChanged(this);
    
  }
  public DeviderChange( final IProfilDevider devider, final DEVIDER_PROPERTY property,
      final Object newValue )
  {
    super( devider, property, newValue );
  }
  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profil.changes.AbstractChange#doChange(PlainProfil)
   */
  @Override
  public void doChange(PlainProfil profil) throws ProfilDataException
  {
    final ProfilDevider d = (ProfilDevider)m_object;
    m_oldValue = d.getValueFor(m_property);
    d.setValueFor( m_property, m_newValue );
  
  }
  /**
   * @see com.bce.eind.core.profil.changes.AbstractChange#getUndoChange()
   */
  @Override
  public AbstractChange getUndoChange( ) throws ProfilDataException
  {
    final IProfilDevider d = (IProfilDevider)m_object;
    final DEVIDER_PROPERTY dp = (DEVIDER_PROPERTY)m_property;
    return new DeviderChange( d, dp, m_oldValue );
  }
}
