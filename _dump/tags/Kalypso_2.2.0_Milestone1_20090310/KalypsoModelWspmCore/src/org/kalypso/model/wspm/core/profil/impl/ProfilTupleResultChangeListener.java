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

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyEdit;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.ITupleResultChangedListener;

/**
 * Listens on the tuple result and translates its changes into profile change events.
 * 
 * @author Gernot Belger
 * @author kimwerner
 */
public class ProfilTupleResultChangeListener implements ITupleResultChangedListener
{
  private final IProfil m_profil;

  public ProfilTupleResultChangeListener( final IProfil profil )
  {
    m_profil = profil;
  }

  /**
   * @see org.kalypso.observation.result.ITupleResultChangedListener#componentsChanged(org.kalypso.observation.result.IComponent[],
   *      org.kalypso.observation.result.ITupleResultChangedListener.TYPE)
   */
  public void componentsChanged( final IComponent[] components, final TYPE type )
  {
    // do nothing

  }

  /**
   * @see org.kalypso.observation.result.ITupleResultChangedListener#recordsChanged(org.kalypso.observation.result.IRecord[],
   *      org.kalypso.observation.result.ITupleResultChangedListener.TYPE)
   */
  public void recordsChanged( final IRecord[] records, final TYPE type )
  {
    if( type == TYPE.ADDED )
    {
      /*
       * we only need a refresh here, so fire a "null-change"
       */
      final ProfilChangeHint hint = new ProfilChangeHint();
      hint.setProfilPropertyChanged( true );
      m_profil.fireProfilChanged( hint, new IProfilChange[] { null } );
    }

  }

  /**
   * @see org.kalypso.observation.result.ITupleResultChangedListener#valuesChanged(org.kalypso.observation.result.ITupleResultChangedListener.ValueChange[])
   */
  public void valuesChanged( final ValueChange[] changes )
  {
    final ProfilChangeHint hint = new ProfilChangeHint();

    final List<IProfilChange> profChanges = new ArrayList<IProfilChange>();

    for( final ValueChange change : changes )
    {
      final IComponent component = m_profil.getResult().getComponent( change.getComponent() );

      final PointPropertyEdit pointPropertyEdit = new PointPropertyEdit( change.getRecord(), component, change.getNewValue() );
      profChanges.add( pointPropertyEdit );

      hint.setPointValuesChanged();
      if( m_profil.isPointMarker( component.getId() ) )
        hint.setMarkerMoved();
    }

    m_profil.fireProfilChanged( hint, profChanges.toArray( new IProfilChange[profChanges.size()] ) );

  }

}
