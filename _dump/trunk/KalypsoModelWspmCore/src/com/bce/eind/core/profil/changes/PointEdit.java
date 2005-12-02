package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfilListener;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;
import com.bce.eind.core.profil.impl.PlainProfil;
import com.bce.eind.core.profil.impl.points.ProfilPoint;

public final class PointEdit extends AbstractChange
{

  /**
   * @see com.bce.eind.core.profil.changes.AbstractChange#fireEvent(com.bce.eind.core.profil.IProfilListener)
   */
  @Override
  public void fireEvent( IProfilListener listener )
  {
    listener.onPointsChanged(this);
    
  }

  public PointEdit( final IProfilPoint p, final POINT_PROPERTY property, final Double newValue )
  {
    super( p, property, newValue );
  }

  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profil.changes.AbstractChange#doChange(PlainProfil)
   */
  @Override
  public void doChange(PlainProfil profil ) throws ProfilDataException
  {
    final ProfilPoint p = (ProfilPoint)m_object;
    final POINT_PROPERTY pp=(POINT_PROPERTY)m_property;
    m_oldValue = p.getValueFor(pp);
    p.setValueFor(pp , (Double)m_newValue );
    
  }

  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profil.changes.AbstractChange#getUndoChange()
   */
  @Override
  public PointEdit getUndoChange( ) throws ProfilDataException
  {
    final IProfilPoint p = (IProfilPoint)m_object;
    final POINT_PROPERTY pp = (POINT_PROPERTY)m_property;
    return new PointEdit( p, pp, (Double)m_oldValue );
  }
}