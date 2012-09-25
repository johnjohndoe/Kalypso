/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Bj�rnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universit�t Hamburg-Harburg, Institut f�r Wasserbau, Hamburg, Germany
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

import java.io.IOException;
import java.net.URI;
import java.util.Date;
import java.util.LinkedHashSet;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.hibernate.Session;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.pdb.PdbUtils;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.GetPdbList;
import org.kalypso.model.wspm.pdb.db.constants.StateConstants.ZeroState;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPartType;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.db.utils.StateUtils;
import org.kalypso.model.wspm.pdb.db.utils.WaterBodyUtils;
import org.kalypso.model.wspm.pdb.gaf.GafCodes;
import org.kalypso.model.wspm.pdb.gaf.ICoefficients;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;

/**
 * Checks if the state is new or will be replaced.<br/>
 * Also opens the session and keeps track of it.
 *
 * @author Gernot Belger
 */
public class CheckinStatePrepareOperation implements ICoreRunnableWithProgress
{
  private Session m_session;

  private final CheckinStateData m_data;

  private State m_state;

  private State m_existingState;

  private ICheckinStatePdbOperation m_operation;

  public CheckinStatePrepareOperation( final CheckinStateData data )
  {
    m_data = data;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      /* open new session */
      final IPdbConnection connection = m_data.getConnection();
      m_session = connection.openSession();

      /* clone state, to avoid attaching this state object to a session; else we get problems if the operations fails, the state will remain attached to the session */
      m_state = new State( m_data.getState() );
      m_state.setEditingUser( connection.getSettings().getUsername() );
      m_state.setEditingDate( new Date() );

      /* Re-read water body, to make sure it is from the same session */
      final String waterCode = m_data.getWaterBody().getName();
      final WaterBody waterBody = WaterBodyUtils.findWaterBody( m_session, waterCode );
      if( waterBody == null )
        throw new PdbConnectException( String.format( "Water body '%s' was not found in database", waterCode ) );

      /* gather data for operation */
      final CheckinStateOperationData operationData = createOperationData( m_session, waterBody );

      /* check status of state */
      m_existingState = StateUtils.findStateByName( m_session, m_state.getName() );
      m_operation = createOperation( operationData, m_existingState, m_state );

      if( m_existingState != null )
        m_state.setId( m_existingState.getId() );

      return Status.OK_STATUS;
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, Messages.getString( "CheckinStateOperation.2" ), e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();

      final IStatus status = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, "Fehler beim Zugriff auf die Datenbank", e );
      throw new CoreException( status );
    }
    finally
    {

    }
  }

  private CheckinStateOperationData createOperationData( final Session session, final WaterBody waterBody ) throws IOException, PdbConnectException
  {
    final TuhhReach reach = m_data.getReach();
    final IProfileFeature[] profiles = findProfiles( reach );

    final GafCodes gafCodes = new GafCodes();

    final String dbSrs = m_data.getDatabaseSrs();
    final ICoefficients coefficients = m_data.getCoefficients();
    final URI documentBase = m_data.getDocumentBase();

    final CrossSectionPartType[] partTypes = GetPdbList.getArray( session, CrossSectionPartType.class );

    return new CheckinStateOperationData( partTypes, gafCodes, coefficients, waterBody, reach, profiles, dbSrs, documentBase );
  }

  private ICheckinStatePdbOperation createOperation( final CheckinStateOperationData operationData, final State existingState, final State newState )
  {
    if( existingState == null )
    {
      /* new state */
      return createNewOperation( operationData, newState );
    }

    if( existingState.getIsstatezero() == ZeroState.T )
    {
      /* existing zero is write protected -> no operation! */
      return null;
    }
    else
    {
      /* existing state will be overwritten */
      return createUpdateOperation( operationData );
    }
  }

  private ICheckinStatePdbOperation createNewOperation( final CheckinStateOperationData operationData, final State newState )
  {
    return new CheckinStatePdbOperation( operationData, newState, true );
  }

  private ICheckinStatePdbOperation createUpdateOperation( final CheckinStateOperationData operationData )
  {
    // TODO Auto-generated method stub
    return null;
  }

  public void dispose( )
  {
    PdbUtils.closeSessionQuietly( m_session );
  }

  public boolean warnUser( final Shell shell, final String windowTitle )
  {
    if( m_existingState == null )
    {
      /* new state, no need for silly questions */
      return true;
    }

    if( m_existingState.getIsstatezero() == ZeroState.T )
    {
      /* existing zero is write protected -> no operation! */
      MessageDialog.openError( shell, windowTitle, CheckinStateValidator.STR_STATE_ISZERO );
      return false;
    }
    else
    {
      /* existing state will be overwritten */
      return MessageDialog.openConfirm( shell, windowTitle, CheckinStateValidator.STR_STATE_WILL_BE_OVERWRITTEN );
    }
  }

  public ICoreRunnableWithProgress createCheckinOperation( )
  {
    return new CheckinStateOperation( m_session, m_data, m_state, m_operation );
  }

  private IProfileFeature[] findProfiles( final TuhhReach reach )
  {
    final Set<IProfileFeature> profiles = new LinkedHashSet<>();

    final TuhhReachProfileSegment[] segments = reach.getReachProfileSegments();
    for( final TuhhReachProfileSegment segment : segments )
    {
      final IProfileFeature profile = segment.getProfileMember();
      profiles.add( profile );
    }

    return profiles.toArray( new IProfileFeature[profiles.size()] );
  }
}