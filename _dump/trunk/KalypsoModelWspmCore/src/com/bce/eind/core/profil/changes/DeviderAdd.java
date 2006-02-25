package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.IProfilDevider;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDeviderFactory;
import com.bce.eind.core.profil.IProfilDevider.DEVIDER_TYP;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;

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
   * @see com.bce.eind.core.profil.changes.AbstractChange#doChange(PlainProfil)
   */
  public IProfilChange doChange( final ProfilChangeHint hint )
  {
    hint.setDeviderMoved();

    m_profil.addDevider( m_devider );
    return new DeviderRemove( m_profil, m_devider );
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
