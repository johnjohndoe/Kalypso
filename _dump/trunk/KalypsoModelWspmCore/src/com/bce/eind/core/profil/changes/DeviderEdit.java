package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.IProfilDevider;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilDevider.DEVIDER_PROPERTY;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;
import com.bce.eind.core.profil.impl.PlainProfil;

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
  public IProfilChange doChange( final ProfilChangeHint hint ) throws ProfilDataException
  {
    hint.setDeviderDataChanged();
    
    final Object oldValue = m_devider.getValueFor( m_property );
    m_devider.setValueFor( m_property, m_newValue );
    
    return new DeviderEdit( m_devider, m_property, oldValue );
  }

  /* (non-Javadoc)
   * @see com.bce.eind.core.profil.IProfilChange#getChangedPoint()
   */
  public IProfilPoint getChangedPoint( )
  {
    return m_devider.getPoint();
  }

  /* (non-Javadoc)
   * @see com.bce.eind.core.profil.IProfilChange#getChangedProperty()
   */
  public POINT_PROPERTY getChangedProperty( )
  {
    return null;
  }
}
