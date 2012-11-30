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

import java.util.Collection;

import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.State;

/**
 * Simple implementation of {@link IEditEventPageData} with fixed return values.
 * 
 * @author Gernot Belger
 */
public class DefaultEditEventPageData extends AbstractModelObject implements IEditEventPageData
{
  private final Event m_event;

  private final Collection<State> m_states;

  private final boolean m_showStatesChooser;

  private final ExistingEventsFetcher m_eventsFetcher;

  public DefaultEditEventPageData( final ExistingEventsFetcher eventsFetcher, final Event event, final Collection<State> states, final boolean showStatesChooser )
  {
    m_eventsFetcher = eventsFetcher;
    m_event = event;
    m_states = states;
    m_showStatesChooser = showStatesChooser;
  }

  @Override
  public Event getEvent( )
  {
    return m_event;
  }

  @Override
  public Event[] getExistingEvents( ) throws PdbConnectException
  {
    if( m_eventsFetcher == null )
    {
      // no check needed, new state is created (during gaf import), event name may be arbitrary
      return null;
    }

    return m_eventsFetcher.getEvents();
  }

  @Override
  public Collection<State> getStates( )
  {
    return m_states;
  }

  @Override
  public boolean showStatesChooser( )
  {
    return m_showStatesChooser;
  }
}