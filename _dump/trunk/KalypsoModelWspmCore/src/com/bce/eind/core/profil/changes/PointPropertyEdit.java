package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;

public final class PointPropertyEdit implements IProfilChange
{
  private final IProfilPoint m_point;

  private final POINT_PROPERTY m_property;

  private final Double m_newValue;

  public PointPropertyEdit( final IProfilPoint p, final POINT_PROPERTY property, final Double newValue )
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
    if (hint!=null) hint.setPointValuesChanged();
    
    final Double oldValue = m_point.getValueFor( m_property );
    m_point.setValueFor( m_property, m_newValue );

    return new PointPropertyEdit( m_point, m_property, oldValue );
  }

  /**
   * @see com.bce.eind.core.profil.IProfilChange#getObject()
   */
  public Object getObject( )
  {
        return m_point;
  }

  /**
   * @see com.bce.eind.core.profil.IProfilChange#getPointProperty()
   */
  public POINT_PROPERTY getPointProperty( )
  {
        return m_property;
  }

  /**
   * @see com.bce.eind.core.profil.IProfilChange#getValue()
   */
  public Double getValue( )
  {
       return m_newValue;
  }

 
}