/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.pdb.db.utils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.ObjectUtils;
import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.criterion.Restrictions;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;

/**
 * @author Gernot Belger
 */
public final class EventUtils
{
  private EventUtils( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * Load the event with the given name from the session
   */
  public static Event findEventByName( final Session session, final String name )
  {
    final Criteria criteria = session.createCriteria( Event.class );
    criteria.add( Restrictions.eq( Event.PROPERTY_NAME, name ) );
    return (Event)criteria.uniqueResult();
  }

  public static Event findEventByName( final Event[] events, final String name )
  {
    for( final Event event : events )
    {
      if( ObjectUtils.equals( event.getName(), name ) )
        return event;
    }
    return null;
  }

  /**
   * Find the state of the given event from the db (by name), and returns its sections
   */
  public static Set<CrossSection> loadSectionsForStateName( final Session session, final Event event )
  {
    if( event == null )
      return null;

    final State state = event.getState();
    if( state == null )
      return null;

    final String stateName = state.getName();

    /* load real event from db */
    final State dbState = StateUtils.findStateByName( session, stateName );
    if( dbState == null )
      return null;

    /* hash its sections */
    return dbState.getCrossSections();
  }

  public static Map<String, Event> loadEventMap( final Session session, final WaterBody waterBody, final State state, final String eventName )
  {
    /* find all for given waterbody and state */
    final Criteria criteria = session.createCriteria( Event.class );

    criteria.add( Restrictions.eq( Event.PROPERTY_NAME, eventName ) );
    criteria.add( Restrictions.eq( Event.PROPERTY_STATE, state ) );
    criteria.add( Restrictions.eq( Event.PROPERTY_WATER_BODY, waterBody ) );

    final List<Event> list = criteria.list();

    /* hash by name */
    final Map<String, Event> result = new HashMap<>( list.size() );

    for( final Event event : list )
      result.put( event.getName(), event );

    return result;
  }

  public static Event[] loadEvents( final Session session, final WaterBody waterBody )
  {
    /* find all for given waterbody */
    final Criteria criteria = session.createCriteria( Event.class );

    criteria.add( Restrictions.eq( Event.PROPERTY_WATER_BODY, waterBody ) );

    final List<Event> list = criteria.list();
    return list.toArray( new Event[list.size()] );
  }
}