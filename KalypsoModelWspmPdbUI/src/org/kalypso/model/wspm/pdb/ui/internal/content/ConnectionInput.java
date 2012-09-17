/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
import java.util.List;

import org.hibernate.Session;
import org.kalypso.model.wspm.pdb.PdbUtils;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.GetPdbList;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;

/**
 * @author Gernot Belger
 */
public class ConnectionInput
{
  private final WaterBodyStructure m_waters;

  private final Session m_session;

  private final List<State> m_states;

  public ConnectionInput( final Session session ) throws PdbConnectException
  {
    m_session = session;

    m_states = readState();
    final List<WaterBody> waterBodies = readWaterBody();

    m_waters = new WaterBodyStructure( waterBodies );
  }

  private List<State> readState( ) throws PdbConnectException
  {
      return GetPdbList.getList( m_session, State.class );
  }

  private List<WaterBody> readWaterBody( )
  {
    try
    {
      return GetPdbList.getList( m_session, WaterBody.class );
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
      return new ArrayList<>();
    }
  }

  public void dispose( )
  {
    // TODO: error handling
    PdbUtils.closeSessionQuietly( m_session );
  }

  public State[] getState( )
  {
    return m_states.toArray( new State[m_states.size()] );
  }

  public State getState( final String name )
  {
    if( name == null )
      return null;

    for( final State state : m_states )
    {
      if( name.equals( state.getName() ) )
        return state;
    }

    return null;
  }

  public WaterBody getWaterBody( final String waterBodyName )
  {
    if( waterBodyName == null )
      return null;

    return m_waters.findWaterBodyByName( waterBodyName );
  }

  public Event getEvent( final String eventName )
  {
    if( eventName == null )
      return null;
    // FIXME: not unique, so we may find the wrong one here!
    return m_waters.findEventName( eventName );
  }

  public WaterBodyStructure getStructure( )
  {
    return m_waters;
  }
}