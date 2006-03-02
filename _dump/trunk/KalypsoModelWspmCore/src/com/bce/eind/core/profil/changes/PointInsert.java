package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;
import com.bce.eind.core.profil.util.ProfilUtil;

public class PointInsert implements IProfilChange
{
  private final IProfilPoint m_thePointBefore;

  private IProfilPoint m_newPoint;

  private final IProfil m_profil;

  public PointInsert( final IProfil profil, final IProfilPoint thePointBefore )
  {
    m_profil = profil;
    m_thePointBefore = thePointBefore;
  }

  public IProfilChange doChange( final ProfilChangeHint hint ) throws ProfilDataException
  {
    if (hint!=null) hint.setPointsChanged();

    final IProfilPoint thePointAfter = ProfilUtil.getPointAfter( m_profil, m_thePointBefore );
    if( thePointAfter == null )
      // throw new ProfilDataException( "Kann Punkt am Ende nicht einfügen." );
      m_newPoint = m_thePointBefore.clonePoint();
    else m_newPoint = ProfilUtil.splitSegment( m_thePointBefore, thePointAfter );

    m_profil.insertPoint( m_thePointBefore, m_newPoint );

    return new PointRemove( m_profil, m_newPoint );
  }

  /**
   * @see com.bce.eind.core.profil.IProfilChange#getObject()
   */
  public Object getObject( )
  {
    return m_newPoint;
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
