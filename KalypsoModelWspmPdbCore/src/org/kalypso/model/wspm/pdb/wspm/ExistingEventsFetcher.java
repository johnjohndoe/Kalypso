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

import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang3.tuple.Pair;
import org.hibernate.Session;
import org.kalypso.model.wspm.pdb.PdbUtils;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.db.utils.EventUtils;

/**
 * @author Gernot Belger
 */
public class ExistingEventsFetcher
{
  private final IPdbConnection m_connection;

  private final Event m_event;

  /* all events of current water body, cached to reduce database queries */
  private Pair<WaterBody, Event[]> m_existingEvents = null;

  public ExistingEventsFetcher( final IPdbConnection connection, final Event event )
  {
    m_connection = connection;
    m_event = event;
  }

  public Event[] getEvents( ) throws PdbConnectException
  {
    final State state = m_event.getState();
    final WaterBody waterBody = m_event.getWaterBody();

    /* already in cache ? */
    if( m_existingEvents != null && m_existingEvents.getKey() == waterBody )
      return filterEvents( m_existingEvents.getValue(), state );

    /* update cache from db */
    Session session = null;
    try
    {
      session = m_connection.openSession();

      final Event[] events = EventUtils.loadEvents( session, waterBody );

      // REMARK: directly fetch states, as we need an open session to do so
      for( final Event event : events )
      {
        final State eventState = event.getState();
        if( eventState != null )
          eventState.getName();
      }

      m_existingEvents = Pair.of( waterBody, events );

      session.close();

      return filterEvents( events, state );
    }
    finally
    {
      PdbUtils.closeSessionQuietly( session );
    }
  }

  private Event[] filterEvents( final Event[] value, final State state )
  {
    final Collection<Event> filteredEvents = new ArrayList<>();

    for( final Event event : value )
    {
      if( isEventOfState( event, state ) )
        filteredEvents.add( event );
    }

    return filteredEvents.toArray( new Event[filteredEvents.size()] );
  }

  private boolean isEventOfState( final Event event, final State state )
  {
    final State eventState = event.getState();
    if( state == null && eventState == null )
      return true;

    if( state == null && eventState != null )
      return false;

    if( state != null && eventState == null )
      return false;

    return eventState.getName().equals( state.getName() );
  }
}