/**
 * 
 */
package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;
import com.bce.eind.core.profil.util.ProfilUtil;

/**
 * @author kimwerner
 */
public class PointRemove implements IProfilChange
{
  private final IProfil m_profil;

  private final IProfilPoint m_point;

  private IProfilPoint m_pointBefore;

  public PointRemove( final IProfil profil, final IProfilPoint point )
  {
    m_profil = profil;
    m_point = point;
  }

  /**
   * @see com.bce.eind.core.profil.IProfilChange#doChange()
   */
  public IProfilChange doChange( final ProfilChangeHint hint ) throws ProfilDataException
  {
    hint.setPointsChanged();
    
    m_pointBefore = ProfilUtil.getPointBefore( m_profil, m_point );
    
    m_profil.removePoint( m_point );

    return new PointAdd( m_profil, m_pointBefore, m_point );
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
