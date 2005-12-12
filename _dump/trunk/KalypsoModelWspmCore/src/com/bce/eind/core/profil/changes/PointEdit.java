package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;
import com.bce.eind.core.profil.impl.PlainProfil;

public final class PointEdit implements IProfilChange
{
  private final IProfilPoint m_point;

  private final POINT_PROPERTY m_property;

  private final Double m_newValue;

  public PointEdit( final IProfilPoint p, final POINT_PROPERTY property, final Double newValue )
  {
    m_point = p;
    m_property = property;
    m_newValue = newValue;
  }

  /**
   * @throws ProfilDataException
   * @see com.bce.eind.core.profil.changes.AbstractChange#doChange(PlainProfil)
   */
  public IProfilChange doChange( final ProfilChangeHint hint ) throws ProfilDataException
  {
    hint.setPointValuesChanged();
    
    final Double oldValue = m_point.getValueFor( m_property );
    m_point.setValueFor( m_property, m_newValue );

    return new PointEdit( m_point, m_property, oldValue );
  }

  /* (non-Javadoc)
   * @see com.bce.eind.core.profil.IProfilChange#getChangedPoint()
   */
  public IProfilPoint getChangedPoint( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /* (non-Javadoc)
   * @see com.bce.eind.core.profil.IProfilChange#getChangedProperty()
   */
  public POINT_PROPERTY getChangedProperty( )
  {
    // TODO Auto-generated method stub
    return null;
  }
}