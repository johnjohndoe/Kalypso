package com.bce.eind.core.profil.changes;

import java.util.LinkedList;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;

public final class PointPropertyAdd implements IProfilChange
{
  private final IProfil m_profil;

  private final POINT_PROPERTY m_property;

  private final double[] m_values;

  public PointPropertyAdd( final IProfil profil, final POINT_PROPERTY property,
      final double[] values )
  {
    m_profil = profil;
    m_property = property;
    m_values = values;
  }

  public PointPropertyAdd( final IProfil profil, final POINT_PROPERTY property,
      final double defaultValue )
  {
    m_profil = profil;
    m_property = property;
    m_values = new double[profil.getPoints().size()];
    for( int i = 0; i < m_values.length; i++ )
    {
      m_values[i] = defaultValue;
    }
  }

  public IProfilChange doChange( final ProfilChangeHint hint )
  {
    hint.setPointPropertiesChanged();
    
    m_profil.getProfilPoints().addProperty( m_property );
    final LinkedList<IProfilPoint> points = m_profil.getPoints();
    int i = 0;
    for( IProfilPoint point : points )
    {
      point.setValueFor( m_property, m_values[i++] );

    }
    return new PointPropertyRemove( m_profil, m_property );
  }

  /**
   * @see com.bce.eind.core.profil.IProfilChange#getObject()
   */
  public Object getObject( )
  {
    // TODO Auto-generated method stub
    return null;
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