/**
 * 
 */
package org.kalypso.model.wspm.core.profil.changes;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;


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
   * @see org.kalypso.model.wspm.core.profil.IProfilChange#doChange()
   */
  public IProfilChange doChange( final ProfilChangeHint hint ) throws ProfilDataException
  {
    if( hint != null )
      hint.setPointsChanged();

    m_pointBefore = ProfilUtil.getPointBefore( m_profil, m_point );

    if( m_profil.removePoint( m_point ) )

      return new PointAdd( m_profil, m_pointBefore, m_point );
    else
    {
      final double b = m_point.getValueFor( POINT_PROPERTY.BREITE );
      return new IllegalChange( "Punkt bei [" + Double.toString( b ) + "] kann nicht gelöscht werden" );
    }
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
