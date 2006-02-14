package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;

public final class PointPropertyRemove implements IProfilChange
{
  private final IProfil m_profil;

  private final POINT_PROPERTY m_property;

  public PointPropertyRemove( final IProfil profil, final POINT_PROPERTY property )
  {
    m_profil = profil;
    m_property = property;

  }

  public IProfilChange doChange( final ProfilChangeHint hint ) throws ProfilDataException
  {
    hint.setPointPropertiesChanged();

    final double[] oldValue = m_profil.getValuesFor( m_property );
    m_profil.getProfilPoints().removeProperty( m_property );

    return new PointPropertyAdd( m_profil, m_property, oldValue );
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