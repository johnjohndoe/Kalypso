/**
 * 
 */
package org.kalypso.model.wspm.core.profil.changes;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPoints;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;


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
    m_point = (point == null) ? ProfilUtil.getProfilPoint( profil, pointBefore, null ) : point;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#doChange()
   */
  public IProfilChange doChange( final ProfilChangeHint hint ) throws ProfilDataException
  {
    if( hint != null )
      hint.setPointsChanged();
    final IProfilPoints points = m_profil.getProfilPoints();
    points.insertPoint( m_pointBefore, m_point );
    return new PointRemove( m_profil, m_point );
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#getObject()
   */
  public Object getObject( )
  {
    return m_point;
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
