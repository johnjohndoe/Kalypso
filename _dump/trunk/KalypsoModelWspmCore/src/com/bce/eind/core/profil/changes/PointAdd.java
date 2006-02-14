/**
 * 
 */
package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.IProfilPoints;
import com.bce.eind.core.profil.ProfilDataException;

/**
 * @author kimwerner
 */
public class PointAdd implements IProfilChange
{
  private final IProfil m_profil;

  private final IProfilPoint m_pointBefore;

  private final IProfilPoint m_point;

  public PointAdd( final IProfil profil, final IProfilPoint pointBefore, final IProfilPoint point )
  {
    m_profil = profil;
    m_pointBefore = pointBefore;
    m_point = point;
  }

  /**
   * @see com.bce.eind.core.profil.IProfilChange#doChange()
   */
  public IProfilChange doChange( final ProfilChangeHint hint ) throws ProfilDataException
  {
    hint.setPointsChanged();
    final IProfilPoints points = m_profil.getProfilPoints();
    if( m_point == null )
    {
      return new PointRemove( m_profil, points.addPoint( 0, 0 ) );
    }
    else
    {
      points.insertPoint( m_pointBefore, m_point );
      return new PointRemove( m_profil, m_point );
    }
  }
}
