/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.pdb.wspm;

import java.util.Map;

import org.hibernate.Session;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.db.utils.EventUtils;

/**
 * Helpr that finds the correct waterlevel event for a waterlevel in a cross section. Also caches found events for less db queries.
 * 
 * @author Gernot Belger
 */
public class EventUploadProvider
{
  private final Session m_session;

  private Map<String, Event> m_eventMap;

  public EventUploadProvider( final Session session )
  {
    m_session = session;
  }

  public synchronized Event getEvent( final WaterBody waterBody, final State state, final String eventName )
  {
    if( m_eventMap == null )
      m_eventMap = EventUtils.loadEventMap( m_session, waterBody, state, eventName );

    return m_eventMap.get( eventName );
  }
}