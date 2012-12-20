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
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.databinding.validation.IValidator;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.hibernate.Session;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.pdb.PdbUtils;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.PdbInfo;
import org.kalypso.model.wspm.pdb.db.constants.StateConstants.ZeroState;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.db.utils.StateUtils;
import org.kalypso.model.wspm.pdb.db.utils.WaterBodyUtils;
import org.kalypso.model.wspm.pdb.gaf.ICoefficients;
import org.kalypso.model.wspm.pdb.gaf.IGafConstants;
import org.kalypso.model.wspm.pdb.internal.gaf.Coefficients;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

/**
 * @author Gernot Belger
 */
public class CheckinStateData extends AbstractModelObject
{
  private final State m_state = new State();

  private final CommandableWorkspace m_wspmWorkspace;

  private String m_dbSrs;

  private ICoefficients m_coefficients;

  private final TuhhReach m_reach;

  private URI m_documentBase;

  private IPdbConnection m_connection;

  private WaterBody m_waterBody;

  private IValidator m_stateNameValidator;

  public CheckinStateData( final CommandableWorkspace wspmWorkspace, final TuhhReach reach )
  {
    m_wspmWorkspace = wspmWorkspace;
    m_reach = reach;

    /* Initialize state data */
    final Date now = new Date();

    m_state.setMeasurementDate( now );
    m_state.setName( reach.getName() );
    m_state.setDescription( reach.getDescription() );
    m_state.setIsstatezero( State.ZeroState.F );
    m_state.setSource( Messages.getString( "CheckInEventData_0" ) ); //$NON-NLS-1$
  }

  public void init( final IPdbConnection connection ) throws PdbConnectException, CoreException
  {
    closeConnection();

    /* prepare for exception */
    m_stateNameValidator = null;

    m_connection = connection;

    Session session = null;
    try
    {
      session = connection.openSession();

      final PdbInfo info = new PdbInfo( session );

      /* what is the relevant water body ? */
      final WspmWaterBody wspmWaterBody = m_reach.getWaterBody();
      final String waterCode = wspmWaterBody.getRefNr();
      final String stateName = m_reach.getName();

      m_waterBody = WaterBodyUtils.findWaterBody( session, waterCode );

      /* state with same name in database */
      final State existingState = StateUtils.findStateByName( session, stateName );

      m_stateNameValidator = initUpdateStates( session, m_waterBody, existingState, stateName );

      initFromExistingState( existingState );

      m_coefficients = new Coefficients( session, IGafConstants.POINT_KIND_GAF );

      session.close();

      m_dbSrs = JTSAdapter.toSrs( info.getSRID() );

      m_documentBase = info.getDocumentBase();
    }
    finally
    {
      PdbUtils.closeSessionQuietly( session );
    }
  }

  /**
   * Copy some values from existing state into new state, so they won't get lost.<br/>
   * Especially data, that is not present in wspm.
   */
  private void initFromExistingState( final State existingState )
  {
    if( existingState == null )
      return;

    m_state.setCreationDate( existingState.getCreationDate() );
    m_state.setEditingDate( existingState.getEditingDate() );
    m_state.setEditingUser( existingState.getEditingUser() );
    m_state.setMeasurementDate( existingState.getMeasurementDate() );
    m_state.setSource( existingState.getSource() );
  }

  private IValidator initUpdateStates( final Session session, final WaterBody waterBody, final State existingState, final String reachName )
  {
    if( waterBody == null )
      return null;

    /* all state names in db */
    final Set<String> allStateNames = new HashSet<>( Arrays.asList( StateUtils.getStateNames( session ) ) );

    /* states in same water body */
    final State[] states = WaterBodyUtils.getStatesForWaterByID( session, waterBody.getId() );
    final Set<String> sisterStateNames = new HashSet<>( states.length );
    for( final State state : states )
      sisterStateNames.add( state.getName() );

    /* remove from this set: is updateable in theorie, but name changed so it cannot be overwritten */
    if( existingState == null )
      return new CheckinStateValidator( allStateNames, sisterStateNames, null, IStatus.CANCEL, StringUtils.EMPTY );

    if( existingState.getIsstatezero() == ZeroState.T )
      return new CheckinStateValidator( allStateNames, sisterStateNames, reachName, IStatus.ERROR, CheckinMessages.STR_STATE_ISZERO );

    return new CheckinStateValidator( allStateNames, sisterStateNames, reachName, IStatus.INFO, CheckinMessages.STR_STATE_WILL_BE_OVERWRITTEN );
  }

  public State getState( )
  {
    return m_state;
  }

  /**
   * The water body the state will get cheecked in; found via code of element to be checked in
   */
  public WaterBody getWaterBody( )
  {
    return m_waterBody;
  }

  public Object getProject( )
  {
    return m_wspmWorkspace;
  }

  public String getDatabaseSrs( )
  {
    return m_dbSrs;
  }

  public ICoefficients getCoefficients( )
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

  public IValidator getStateNameValidator( )
  {
    return m_stateNameValidator;
  }
}