package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfilDevider;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilDevider.DEVIDER_PROPERTY;
import com.bce.eind.core.profil.impl.devider.ProfilDevider;

public class DeviderChange extends AbstractChange
{

  public DeviderChange( final IProfilDevider devider, final DEVIDER_PROPERTY property,
      final Object newValue )
  {
    super( devider, property, newValue );
  }
  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profil.changes.AbstractChange#doChange(com.bce.eind.core.profil.IProfil)
   */
  @Override
  public EventToFire doChange() throws ProfilDataException
  {
    ((ProfilDevider)m_object).setValueFor( m_property, m_newValue );
    if( m_object == null )
      return EventToFire.DEVIDER_ADD;

    if( m_newValue == null )
      return EventToFire.DEVIDER_REMOVED;

    return EventToFire.DEVIDER_CHANGED;
  }
  /**
   * @see com.bce.eind.core.profil.changes.AbstractChange#getUndoChange()
   */
  @Override
  public AbstractChange getUndoChange( ) throws ProfilDataException
  {
    final IProfilDevider d = (IProfilDevider)m_object;
    final DEVIDER_PROPERTY dp = (DEVIDER_PROPERTY)m_property;
    final Object oldValue = d.getValueFor( dp );
    return new DeviderChange( d, dp, oldValue );
  }
}
