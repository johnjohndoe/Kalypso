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
package org.kalypso.model.wspm.pdb.db.utils;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.hibernate.Criteria;
import org.hibernate.FetchMode;
import org.hibernate.Session;
import org.hibernate.criterion.Restrictions;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.wspm.IEditEventPageData;

/**
 * @author Gernot Belger
 */
public final class WaterBodyUtils
{
  private WaterBodyUtils( )
  {
    throw new UnsupportedOperationException();
  }

  public static WaterBody findWaterBodyByName( final WaterBody[] waterbodies, final String name )
  {
    if( name == null )
      return null;

    for( final WaterBody waterBody : waterbodies )
    {
      if( ObjectUtils.equals( waterBody.getName(), name ) )
        return waterBody;
    }
    return null;
  }

  /**
   * Returns the possible {@link State}s an event may have set, including the <code>null</code> state.
   */
  public static Collection<State> getPossibleStates( final WaterBody waterBody )
  {
    if( waterBody == null )
      return Collections.singleton( null );

    final Collection<State> states = waterBody.getStates();

    final Set<State> statesWithNull = new LinkedHashSet<>();
    statesWithNull.add( IEditEventPageData.NO_EVENT_STATE );
    statesWithNull.addAll( states );
    return statesWithNull;
  }

  public static Map<String, WaterBody> hashWaterCodes( final WaterBody[] waterBodies )
  {
    final Map<String, WaterBody> codes = new HashMap<>();

    for( final WaterBody waterBody : waterBodies )
      codes.put( waterBody.getName(), waterBody );

    return Collections.unmodifiableMap( codes );
  }

  public static WaterBody findWaterBody( final Session session, final String waterCode )
  {
    if( StringUtils.isBlank( waterCode ) )
      return null;

    final Criteria criteria = session.createCriteria( WaterBody.class );
    criteria.add( Restrictions.eq( WaterBody.PROPERTY_NAME, waterCode ) );

    return (WaterBody)criteria.uniqueResult();
  }

  /**
   * Query all states of the database that are connected to the given waterbody.<br/>
   * Join-fetches the events of the state.
   */
  public static State[] getStatesForWaterByID( final Session session, final long waterID )
  {
    final WaterBody waterBody = findWaterBodyByID( session, waterID );
    if( waterBody == null )
      return new State[0];

    final Criteria criteria = session.createCriteria( State.class );
    criteria.setFetchMode( State.PROPERTY_EVENTS, FetchMode.JOIN );
    final Criteria csCriteria = criteria.createCriteria( State.PROPERTY_CROSS_SECTIONS );
    csCriteria.add( Restrictions.eq( CrossSection.PROPERTY_WATER_BODY, waterBody ) );

    final Collection<State> list = new HashSet<>( criteria.list() );
    return list.toArray( new State[list.size()] );
  }

  public static WaterBody findWaterBodyByID( final Session session, final long waterID )
  {
    final Criteria criteria = session.createCriteria( WaterBody.class );
    criteria.add( Restrictions.eq( WaterBody.PROPERTY_ID, waterID ) );

    return (WaterBody)criteria.uniqueResult();
  }
}