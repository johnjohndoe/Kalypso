package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;

public class ActiveObjectEdit implements IProfilChange
{
  private final POINT_PROPERTY m_property;

  private final IProfilPoint m_point;

  private final IProfil m_profil;

  public ActiveObjectEdit( final IProfil profil, final IProfilPoint point, final POINT_PROPERTY property )
  {
    m_profil = profil;
    m_property = property;
    m_point = point;
  }

  /**
   * @see com.bce.eind.core.profil.IProfilChange#doChange(ProfilChangeHint)
   */
  public IProfilChange doChange( final ProfilChangeHint hint )
  {
    hint.setActivePointChanged();
    
    final IProfilPoint oldPoint = m_profil.getActivePoint();
    final POINT_PROPERTY oldProperty = m_profil.getActiveProperty();
    m_profil.setActivePoint( m_point );
    m_profil.setActiveProperty( m_property );

    return new ActiveObjectEdit( m_profil, oldPoint, oldProperty );
  }

  /**
   * @see com.bce.eind.core.profil.IProfilChange#getObject()
   */
  public Object getObject( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see com.bce.eind.core.profil.IProfilChange#getPointProperty()
   */
  public POINT_PROPERTY getPointProperty( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see com.bce.eind.core.profil.IProfilChange#getValue()
   */
  public Double getValue( )
  {
    // TODO Auto-generated method stub
    return null;
  }
}
