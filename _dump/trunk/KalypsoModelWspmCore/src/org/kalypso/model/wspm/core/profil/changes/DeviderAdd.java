package org.kalypso.model.wspm.core.profil.changes;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilDeviderFactory;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_TYP;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;

public class DeviderAdd implements IProfilChange
{
  private final IProfil m_profil;

  private final IProfilDevider m_devider;

  public DeviderAdd( final IProfil profil, final IProfilDevider devider )
  {
    m_profil = profil;
    m_devider = devider;
  }

  public DeviderAdd( final IProfil profil, final DEVIDER_TYP typ, final IProfilPoint position )
  {
    m_profil = profil;
    m_devider = ProfilDeviderFactory.createDevider( typ, position );
  }

  /**
   * @throws ProfilDataException
   * @see org.kalypso.model.wspm.core.profil.changes.AbstractChange#doChange(PlainProfil)
   */
  public IProfilChange doChange( final ProfilChangeHint hint )
  {
    if (hint!=null) hint.setDeviderMoved();

    m_profil.addDevider( m_devider );
    return new DeviderRemove( m_profil, m_devider );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getObject()
   */
  public Object getObject( )
  {
        return m_devider;
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
    
    return null;
  }

}
