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

import java.net.URI;
import java.util.Date;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.hibernate.Session;
import org.kalypso.model.wspm.pdb.PdbUtils;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.GetPdbList;
import org.kalypso.model.wspm.pdb.db.PdbInfo;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.gaf.IGafConstants;
import org.kalypso.model.wspm.pdb.internal.gaf.Coefficients;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

/**
 * @author Gernot Belger
 */ 
public class CheckinStateData
{
  private final State m_state = new State();

  private final CommandableWorkspace m_wspmWorkspace;

  private State[] m_existingStates;

  private WaterBody[] m_existingWaterBodies;

  private String m_dbSrs;

  private Coefficients m_coefficients;

  private final TuhhReach m_reach;

  private URI m_documentBase;

  private IPdbConnection m_connection;

  public CheckinStateData( final CommandableWorkspace wspmWorkspace, final TuhhReach reach )
  {
    m_wspmWorkspace = wspmWorkspace;
    m_reach = reach;

    /* Initial state data */
    m_state.setMeasurementDate( new Date() );
    m_state.setName( reach.getName() );
    m_state.setDescription( reach.getDescription() );
    m_state.setIsstatezero( State.ZERO_STATE_OFF );
    m_state.setSource( "WSPM Local Data" );
  }

  public void init( final IPdbConnection connection ) throws PdbConnectException, CoreException
  {
    closeConnection();

    m_connection = connection;

    Session session = null;
    try
    {
      session = connection.openSession();

      final PdbInfo info = new PdbInfo( session );
      final List<State> states = GetPdbList.getList( session, State.class );
      final List<WaterBody> waterbodies = GetPdbList.getList( session, WaterBody.class );

      m_coefficients = new Coefficients( session, IGafConstants.POINT_KIND_GAF );

      session.close();

      m_existingStates = states.toArray( new State[states.size()] );
      m_existingWaterBodies = waterbodies.toArray( new WaterBody[waterbodies.size()] );
      m_dbSrs = JTSAdapter.toSrs( info.getSRID() );

      m_documentBase = info.getDocumentBase();
    }
    finally
    {
      PdbUtils.closeSessionQuietly( session );
    }
  }

  public State getState( )
  {
    return m_state;
  }

  public State[] getExistingStates( )
  {
    return m_existingStates;
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

  public Coefficients getCoefficients( )
  {
    return m_coefficients;
  }

  public TuhhReach getReach( )
  {
    return m_reach;
  }

  public CommandableWorkspace getWorkspace( )
  {
    return m_wspmWorkspace;
  }

  public URI getDocumentBase( )
  {
    return m_documentBase;
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