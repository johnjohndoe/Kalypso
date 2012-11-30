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
package org.kalypso.model.wspm.pdb.wspm;

import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.hibernate.Session;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.model.wspm.pdb.PdbUtils;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.GetPdbList;
import org.kalypso.model.wspm.pdb.db.PdbInfo;
import org.kalypso.model.wspm.pdb.db.constants.EventConstants.TYPE;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.db.utils.WaterBodyUtils;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

/**
 * @author Gernot Belger
 */
public abstract class CheckInEventData<W extends Feature> extends AbstractModelObject implements IEditEventPageData
{
  private final Event m_event = new Event();

  private final CommandableWorkspace m_wspmWorkspace;

  private String m_dbSrs;

  private final W m_wspmObject;

  private IPdbConnection m_connection;

  private Collection<State> m_states;

  private Map<String, WaterBody> m_waterHash;

  private Session m_session;

  private ExistingEventsFetcher m_eventsFetcher;

  public CheckInEventData( final CommandableWorkspace wspmWorkspace, final W wspmObject )
  {
    m_wspmWorkspace = wspmWorkspace;
    m_wspmObject = wspmObject;

    /* Initial state data */
    final Date now = new Date();
    m_event.setMeasurementDate( now );
    m_event.setName( wspmObject.getName() );
    m_event.setDescription( wspmObject.getDescription() );
    m_event.setSource( Messages.getString( "CheckInEventData_0" ) ); //$NON-NLS-1$
    m_event.setEditingDate( now );
    m_event.setType( TYPE.Measurement );
  }

  public void init( final IPdbConnection connection ) throws PdbConnectException
  {
    closeConnection();

    m_connection = connection;

    // REMARK: keep session open until dispose, else we get layz init problems
    m_session = connection.openSession();

    final PdbInfo info = new PdbInfo( m_session );

    m_eventsFetcher = new ExistingEventsFetcher( connection, m_event );

    final List<WaterBody> waterbodies = GetPdbList.getList( m_session, WaterBody.class );
    final WaterBody[] existingWaterBodies = waterbodies.toArray( new WaterBody[waterbodies.size()] );
    m_dbSrs = JTSAdapter.toSrs( info.getSRID() );

    m_waterHash = Collections.unmodifiableMap( WaterBodyUtils.hashWaterCodes( existingWaterBodies ) );

    /* get possible state that can be set to event */
    final WaterBody waterBody = findWaterBody();
    if( waterBody != null )
    {
      // REMAKR: if water body is null, we will get a warning via checkPrecondition later, so just ignore ist here

      // REMARK: must be called with open session
      final Collection<State> possibleStates = WaterBodyUtils.getPossibleStates( waterBody );
      setStates( possibleStates );

      m_event.setWaterBody( waterBody );
    }
  }

  @Override
  public Event getEvent( )
  {
    return m_event;
  }

  @Override
  public Event[] getExistingEvents( ) throws PdbConnectException
  {
    return m_eventsFetcher.getEvents();
  }

  public Object getProject( )
  {
    return m_wspmWorkspace;
  }

  public String getDatabaseSrs( )
  {
    return m_dbSrs;
  }

  public W getWspmObject( )
  {
    return m_wspmObject;
  }

  public CommandableWorkspace getWorkspace( )
  {
    return m_wspmWorkspace;
  }

  public IPdbConnection getConnection( )
  {
    return m_connection;
  }

  public void closeConnection( )
  {
    PdbUtils.closeSessionQuietly( m_session );
    PdbUtils.closeQuietly( m_connection );
  }

  @Override
  public boolean showStatesChooser( )
  {
    return true;
  }

  @Override
  public Collection<State> getStates( )
  {
    return m_states;
  }

  public void setStates( final Collection<State> states )
  {
    m_states = states;
  }

  public Map<String, WaterBody> getWaterHash( )
  {
    return m_waterHash;
  }

  public abstract WaterBody findWaterBody( );
}