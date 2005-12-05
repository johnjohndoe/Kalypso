package com.bce.eind.core.profil.changes;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.util.ProfilUtil;

public class InsertPointAtChange implements IProfilChange
{
  private final IProfilPoint m_thePointBefore;
  private final IProfil m_profil;

  public InsertPointAtChange( final IProfil profil, final IProfilPoint thePointBefore )
  {
    m_profil = profil;
    m_thePointBefore = thePointBefore;
  }

  public IProfilChange doChange( final ProfilChangeHint hint ) throws ProfilDataException
  {
    hint.setPointsChanged();
    
    final IProfilPoint thePointAfter = ProfilUtil.getPointAfter( m_profil, m_thePointBefore );
    if( thePointAfter == null )
      throw new ProfilDataException( "Kann Punkt am Ende nicht einfügen." );
    
    final IProfilPoint newPoint = ProfilUtil.splitSegment( m_thePointBefore, thePointAfter );

    m_profil.insertPoint( m_thePointBefore, newPoint );
    
    return new DeletePointChange( m_profil, newPoint );
  }

}
