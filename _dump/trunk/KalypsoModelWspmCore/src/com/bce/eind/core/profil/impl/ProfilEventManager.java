package com.bce.eind.core.profil.impl;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;

import com.bce.eind.core.ProfilCorePlugin;
import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilChange;
import com.bce.eind.core.profil.IProfilEventManager;
import com.bce.eind.core.profil.IProfilListener;
import com.bce.eind.core.profil.changes.ProfilChangeHint;

/**
 * @author kimwerner
 */
public class ProfilEventManager implements IProfilEventManager
{
  private final List<IProfilListener> m_listeners = new ArrayList<IProfilListener>( 10 );

  private final IProfil m_profil;

  public ProfilEventManager( final IProfil profil )
  {
    m_profil = profil;
  }

  /**
   * @see com.bce.eind.core.profil.impl.IProfilEventManager#getProfil()
   */
  public IProfil getProfil( )
  {
    return m_profil;
  }

  /**
   * @see com.bce.eind.core.profil.impl.IProfilEventManager#addProfilListener(com.bce.eind.core.profil.IProfilListener)
   */
  public void addProfilListener( final IProfilListener pl )
  {
    m_listeners.add( pl );
  }

  /**
   * @see com.bce.eind.core.profil.impl.IProfilEventManager#fireProfilChanged(com.bce.eind.core.profil.changes.ProfilChangeHint,
   *      com.bce.eind.core.profil.IProfilChange[])
   */
  public void fireProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    if( (changes == null) || (changes.length == 0) )
      return;
    final IProfilListener[] listeners = m_listeners.toArray( new IProfilListener[m_listeners.size()] );
    for( final IProfilListener l : listeners )
    {
      try
      {
        l.onProfilChanged( hint, changes );
      }
      catch( final Throwable e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        ProfilCorePlugin.getDefault().getLog().log( status );
      }
    }
  }

  /**
   * @see com.bce.eind.core.profil.impl.IProfilEventManager#removeProfilListener(com.bce.eind.core.profil.IProfilListener)
   */
  public void removeProfilListener( final IProfilListener pl )
  {
    m_listeners.remove( pl );
  }
}