package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.IProfilDevider;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;
import com.bce.eind.core.profil.impl.PlainProfil;

public class DeviderMove implements IProfilChange
{
  private final IProfilDevider m_devider;

  private final IProfilPoint m_newPosition;

  public DeviderMove( IProfilDevider devider, final IProfilPoint newPosition )
  {
    m_devider = devider;
    m_newPosition = newPosition;
  }

  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profil.changes.AbstractChange#doChange(PlainProfil)
   */
  public IProfilChange doChange( final ProfilChangeHint hint ) throws ProfilDataException
  {
    hint.setDeviderMove();

    final IProfilPoint oldPosition = m_devider.setPoint( m_newPosition );

    return new DeviderMove( m_devider, oldPosition );
  }

  /* (non-Javadoc)
   * @see com.bce.eind.core.profil.IProfilChange#getChangedPoint()
   */
  public IProfilPoint getChangedPoint( )
  {
    return m_newPosition;
  }

  /* (non-Javadoc)
   * @see com.bce.eind.core.profil.IProfilChange#getChangedProperty()
   */
  public POINT_PROPERTY getChangedProperty( )
  {
    return null;
  }

 }
