package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.IProfilPoint.PARAMETER;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;

public final class PointPropertyHide implements IProfilChange
{
  private final boolean m_visible;

  private final POINT_PROPERTY m_property;

  public PointPropertyHide(final POINT_PROPERTY property, final boolean visible )
  {
    m_visible = visible;
    m_property = property;

  }

  public IProfilChange doChange( final ProfilChangeHint hint )
  {
    hint.setPointPropertiesChanged();

    final boolean oldValue = (Boolean)m_property.getParameter(PARAMETER.VISIBLE );
    m_property.setVisible(  m_visible);
    return new PointPropertyHide( m_property, oldValue );
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