package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.IProfilDevider;
import com.bce.eind.core.profil.IProfilDevider.DEVIDER_PROPERTY;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;

public class DeviderEdit implements IProfilChange
{
  private final IProfilDevider m_devider;

  private final DEVIDER_PROPERTY m_property;

  private final Object m_newValue;

  public DeviderEdit( final IProfilDevider devider, final DEVIDER_PROPERTY property,
      final Object newValue )
  {
    m_devider = devider;
    m_property = property;
    m_newValue = newValue;
  }

  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profil.changes.AbstractChange#doChange(PlainProfil)
   */
  public IProfilChange doChange( final ProfilChangeHint hint )
  {
    if (hint!=null) hint.setDeviderDataChanged();
    
    final Object oldValue = m_devider.getValueFor( m_property );
    m_devider.setValueFor( m_property, m_newValue );
    
    return new DeviderEdit( m_devider, m_property, oldValue );
  }

  /**
   * @see com.bce.eind.core.profil.IProfilChange#getObject()
   */
  public Object getObject( )
  {
        return m_devider;
  }

  /**
   * @see com.bce.eind.core.profil.IProfilChange#getPointProperty()
   */
  public POINT_PROPERTY getPointProperty( )
  {
        return null;
  }

  /**
   * @see com.bce.eind.core.profil.IProfilChange#getValue()
   */
  public Double getValue( )
  {
    
    return null;
  }

 
}
