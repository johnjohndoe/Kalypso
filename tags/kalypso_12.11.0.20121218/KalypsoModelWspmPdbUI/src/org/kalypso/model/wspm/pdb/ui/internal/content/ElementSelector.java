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
package org.kalypso.model.wspm.pdb.ui.internal.content;

import java.util.ArrayList;
import java.util.Collection;

import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;

/**
 * @author Gernot Belger
 */
public class ElementSelector
{
  private final Collection<String> m_waterBodyNames = new ArrayList<>();

  private final Collection<String> m_stateNames = new ArrayList<>();

  private final Collection<Long> m_eventIds = new ArrayList<>();

  public void addWaterBodyName( final String waterBodyName )
  {
    m_waterBodyNames.add( waterBodyName );
  }

  public void addStateName( final String stateName )
  {
    m_stateNames.add( stateName );
  }

  public void addEventId( final long eventId )
  {
    m_eventIds.add( eventId );
  }

  public Object[] getElements( final ConnectionInput input )
  {
    final Collection<Object> elements = new ArrayList<>();

    for( final String waterBodyName : m_waterBodyNames )
      elements.add( input.getWaterBody( waterBodyName ) );

    for( final String stateName : m_stateNames )
      elements.add( input.getState( stateName ) );

    for( final Long eventId : m_eventIds )
      elements.add( input.getEvent( eventId ) );

    /* null might have been added, remove it now */
    elements.remove( null );

    return elements.toArray( new Object[elements.size()] );
  }

  public void setElemensToSelect( final Object[] elements )
  {
    for( final Object element : elements )
    {
      if( element instanceof WaterBody )
        addWaterBodyName( ((WaterBody)element).getName() );
      else if( element instanceof State )
        addStateName( ((State)element).getName() );
      else if( element instanceof Event )
        addEventId( ((Event)element).getId() );
    }
  }
}