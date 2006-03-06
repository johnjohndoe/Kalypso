package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.IProfilDevider;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;

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
  public IProfilChange doChange( final ProfilChangeHint hint )
  {
    if (hint!=null) hint.setDeviderMoved();

    final IProfilPoint oldPosition = m_devider.setPoint( m_newPosition );

    return new DeviderMove( m_devider, oldPosition );
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
