package com.bce.eind.core.profil.impl;

import java.util.ArrayList;
import java.util.List;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.IProfilListener;
import com.bce.eind.core.profil.changes.ProfilChangeHint;

/**
 * @author kimwerner Basisprofil mit Events, nur die Implementierung von IProfil
 */
public class ProfilEventManager
{
  private final List<IProfilListener> m_listeners = new ArrayList<IProfilListener>( 10 );

  private final IProfil m_profil;

  public ProfilEventManager( final IProfil profil )
  {
    m_profil = profil;
  }

  public IProfil getProfil( )
  {
    return m_profil;
  }

  public void addProfilListener( final IProfilListener pl )
  {
    m_listeners.add( pl );
  }

  public void fireProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    final IProfilListener[] listeners = m_listeners
        .toArray( new IProfilListener[m_listeners.size()] );
    for( final IProfilListener l : listeners )
      l.onProfilChanged( hint, changes );
  }

  public void removeProfilListener( final IProfilListener pl )
  {
    m_listeners.remove( pl );
  }
}