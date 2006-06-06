package org.kalypso.model.wspm.core.profil.changes;

import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;

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
   * @see org.kalypso.model.wspm.core.profil.changes.AbstractChange#doChange(PlainProfil)
   */
  public IProfilChange doChange( final ProfilChangeHint hint )
  {
    if (hint!=null) hint.setDeviderMoved();

    final IProfilPoint oldPosition = m_devider.setPoint( m_newPosition );

    return new DeviderMove( m_devider, oldPosition );
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
