package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;

public final class PointPropertyRemove implements IProfilChange
{
  private final IProfil m_profil;

  private final POINT_PROPERTY m_property;


  public PointPropertyRemove(final IProfil profil, final POINT_PROPERTY property )
  {
    m_profil = profil;
    m_property = property;

  }
  
  
  public IProfilChange doChange( final ProfilChangeHint hint ) throws ProfilDataException
  {
    hint.setPointPropertiesChanged();
//  TODO KIM Dependencies hier entfernen und in di OP verlagerneinfügen
    final double[] oldValue = m_profil.getValuesFor(m_property);
    final POINT_PROPERTY[] pps = m_profil.getDependenciesFor( m_property);
    for(POINT_PROPERTY pp : pps)
      
    {
     m_profil.removeProperty(pp);
    }
   // .removePointProperty(m_property);

    return new PointPropertyAdd( m_profil, m_property, oldValue );
  }
}