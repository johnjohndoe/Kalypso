/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.core.profil.impl;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.wspm.core.KalypsoModelWspmCorePlugin;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilListener;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;

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
        KalypsoModelWspmCorePlugin.getDefault().getLog().log( status );
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