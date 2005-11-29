package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;
import com.bce.eind.core.profil.impl.points.ProfilPoint;

public final class PointChange extends AbstractChange
{

  public PointChange( final IProfilPoint p, final POINT_PROPERTY property, final Double newValue )
  {
    super( p, property, newValue );
  }
  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profil.changes.AbstractChange#doChange(com.bce.eind.core.profil.IProfil)
   */
  @Override
  public boolean doChange() throws ProfilDataException
  {
    ((ProfilPoint)m_object).setValueFor( (POINT_PROPERTY)m_property, (Double)m_newValue );
    return true;
  }
  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profil.changes.AbstractChange#getUndoChange()
   */
  @Override
  public PointChange getUndoChange( ) throws ProfilDataException
  {
    final IProfilPoint p = (IProfilPoint)m_object;
    final POINT_PROPERTY pp = (POINT_PROPERTY)m_property;
    final Double oldValue = p.getValueFor( pp );
    return new PointChange( p, pp, oldValue );
  }
}