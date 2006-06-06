package org.kalypso.model.wspm.core.profil.changes;

import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilPoint.PARAMETER;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;

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
    if (hint!=null) hint.setPointPropertiesChanged();

    final boolean oldValue = (Boolean)m_property.getParameter(PARAMETER.VISIBLE );
    m_property.setVisible(  m_visible);
    return new PointPropertyHide( m_property, oldValue );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getObject()
   */
  public Object getObject( )
  {
       return null;
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