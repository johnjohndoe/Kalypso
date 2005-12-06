package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;

public final class AddPointProperty implements IProfilChange
{
  private final IProfil m_profil;

  private final POINT_PROPERTY m_property;

  private  final double[] m_values;

  public AddPointProperty(final IProfil profil, final POINT_PROPERTY property, final double[] values )
  {
    m_profil = profil;
    m_property = property;
    m_values = values;
  }
  public AddPointProperty( final IProfil profil,final POINT_PROPERTY property, final double defaultValue )
  {
    m_profil = profil;
    m_property = property;
    m_values = new double[profil.getPoints().size()];
    for( int i = 0; i < m_values.length; i++ )
    {
      m_values[i]=defaultValue;
    } 
  }
  
  public IProfilChange doChange( final ProfilChangeHint hint ) throws ProfilDataException
  {
    hint.setPointPropertiesChanged();
    
    final double[] oldValue = m_profil.getValuesFor(m_property);
    m_profil.removePointProperty(m_property);

    return new AddPointProperty( m_profil, m_property, oldValue );
  }
}