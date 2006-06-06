package org.kalypso.model.wspm.core.profil.changes;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;

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
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#doChange(ProfilChangeHint)
   */
  public IProfilChange doChange( final ProfilChangeHint hint )
  {
    if (hint!=null) hint.setActivePointChanged();
    
    final IProfilPoint oldPoint = m_profil.getActivePoint();
    final POINT_PROPERTY oldProperty = m_profil.getActiveProperty();
    m_profil.setActivePoint( m_point );
    m_profil.setActiveProperty( m_property );

    return new ActiveObjectEdit( m_profil, oldPoint, oldProperty );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getObject()
   */
  public Object getObject( )
  {
        return m_point;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getPointProperty()
   */
  public POINT_PROPERTY getPointProperty( )
  {
        return m_property;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getValue()
   */
  public Double getValue( )
  {
       return null;
  }
}
