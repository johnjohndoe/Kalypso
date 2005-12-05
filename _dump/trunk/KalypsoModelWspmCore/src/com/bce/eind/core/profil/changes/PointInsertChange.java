/**
 * 
 */
package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;

/**
 * @author kimwerner
 */
public class PointInsertChange implements IProfilChange
{
  private final IProfil m_profil;

  private final IProfilPoint m_pointBefore;

  private final IProfilPoint m_point;

  public PointInsertChange( final IProfil profil, final IProfilPoint pointBefore,
      final IProfilPoint point )
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
    
    m_profil.insertPoint( m_pointBefore, m_point );
    
    return new DeletePointChange( m_profil, m_point );
  }

}
