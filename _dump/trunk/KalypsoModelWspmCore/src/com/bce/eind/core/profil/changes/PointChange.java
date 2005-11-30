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
  public EventToFire doChange( ) throws ProfilDataException
  {
    ((ProfilPoint)m_object).setValueFor( (POINT_PROPERTY)m_property, (Double)m_newValue );
    if( m_property != null )
    {
      if( m_object == null )

        return EventToFire.POINTS_CHANGED;
      if( m_newValue == null )
        return EventToFire.PROPERTY_REMOVED;
      return EventToFire.PROPERTY_ADD;
    }
    if( m_newValue == null )
      return EventToFire.POINTS_REMOVED;

    return EventToFire.POINTS_ADD;
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