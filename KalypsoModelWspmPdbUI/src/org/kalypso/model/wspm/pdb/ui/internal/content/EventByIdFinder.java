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

import java.util.Collections;
import java.util.Set;

import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;

/**
 * @author Gernot Belger
 */
class EventByIdFinder implements IWaterBodyTreeVisitor
{
  private final long m_searchId;

  private Event m_result;

  public EventByIdFinder( final long searchId )
  {
    m_searchId = searchId;
  }

  @Override
  public void visit( final WaterBodyTreeNode node ) throws CancelException
  {
    final Set<Event> events = getEvents( node );

    for( final Event event : events )
    {
      if( m_searchId == event.getId() )
      {
        m_result = event;
        throw new CancelException();
      }
    }
  }

  private Set<Event> getEvents( final WaterBodyTreeNode node )
  {
    final WaterBody water = node.getWaterBody();
    if( water == null )
      return Collections.emptySet();

    return water.getEvents();
  }

  public Event getResult( )
  {
    return m_result;
  }
}