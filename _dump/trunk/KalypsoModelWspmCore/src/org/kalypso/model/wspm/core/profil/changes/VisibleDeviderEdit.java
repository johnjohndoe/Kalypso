package org.kalypso.model.wspm.core.profil.changes;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_TYP;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;

public class VisibleDeviderEdit implements IProfilChange
{
  private final DEVIDER_TYP m_typ;

  private final boolean m_visible;

  private final IProfil m_profil;

  public VisibleDeviderEdit( IProfil profil, final DEVIDER_TYP typ, final boolean isVisible )
  {
    m_typ = typ;
    m_visible = isVisible;
    m_profil = profil;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#doChange(ProfilChangeHint)
   */
  public IProfilChange doChange( final ProfilChangeHint hint )
  {
    if( hint != null )
      hint.setProfilPropertyChanged( true );
    return new VisibleDeviderEdit( m_profil, m_typ, !m_visible );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getObject()
   */
  public Object getObject( )
  {
    return m_typ;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getPointProperty()
   */
  public POINT_PROPERTY getPointProperty( )
  {
    return null;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getValue()
   */
  public Double getValue( )
  {
    return m_visible ? 1.0 : 0.0;
  }

}
