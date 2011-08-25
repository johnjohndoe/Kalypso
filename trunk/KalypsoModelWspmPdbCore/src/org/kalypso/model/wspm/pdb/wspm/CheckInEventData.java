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

import java.util.Date;
import java.util.List;

import org.hibernate.Session;
import org.kalypso.model.wspm.pdb.PdbUtils;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.GetPdbList;
import org.kalypso.model.wspm.pdb.db.PdbInfo;
import org.kalypso.model.wspm.pdb.db.constants.EventConstants.TYPE;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

/**
 * @author Gernot Belger
 */
public class CheckInEventData<W extends Feature> implements IEditEventPageData
{
  private final Event m_event = new Event();

  private final CommandableWorkspace m_wspmWorkspace;

  private Event[] m_existingEvents;

  private WaterBody[] m_existingWaterBodies;

  private String m_dbSrs;

  private final W m_wspmObject;

  private IPdbConnection m_connection;

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

    Session session = null;
    try
    {
      session = connection.openSession();

      final PdbInfo info = new PdbInfo( session );
      final List<Event> events = GetPdbList.getList( session, Event.class );
      final List<WaterBody> waterbodies = GetPdbList.getList( session, WaterBody.class );

      session.close();

      m_existingEvents = events.toArray( new Event[events.size()] );
      m_existingWaterBodies = waterbodies.toArray( new WaterBody[waterbodies.size()] );
      m_dbSrs = JTSAdapter.toSrs( info.getSRID() );
    }
    finally
    {
      PdbUtils.closeSessionQuietly( session );
    }
  }

  @Override
  public Event getEvent( )
  {
    return m_event;
  }

  @Override
  public Event[] getExistingEvents( )
  {
    return m_existingEvents;
  }

  public WaterBody[] getExistingWaterBodies( )
  {
    return m_existingWaterBodies;
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
    PdbUtils.closeQuietly( m_connection );
  }
}