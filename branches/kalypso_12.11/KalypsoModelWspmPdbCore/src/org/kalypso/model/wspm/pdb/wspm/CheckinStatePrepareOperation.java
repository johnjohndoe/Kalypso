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

import java.net.URI;
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
import org.kalypso.model.wspm.pdb.db.constants.StateConstants.ZeroState;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.db.utils.CrossSectionPartTypes;
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

  private Object m_existingStateZero;

  private CheckinStateOperationData m_operationData;

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

      /* check status of state */
      final State existingState = StateUtils.findStateByName( m_session, m_data.getState().getName() );
      m_existingStateZero = existingState == null ? null : existingState.getIsstatezero();

      /* clone state, to avoid attaching this state object to a session; else we get problems if the operations fails, the state will remain attached to the session */
      final State state = createOrUpdateState( existingState );

      /* Re-read water body, to make sure it is from the same session */
      final String waterCode = m_data.getWaterBody().getName();
      final WaterBody waterBody = WaterBodyUtils.findWaterBody( m_session, waterCode );
      if( waterBody == null )
        throw new PdbConnectException( String.format( Messages.getString("CheckinStatePrepareOperation_0"), waterCode ) ); //$NON-NLS-1$

      /* gather data for operation */
      final String username = connection.getSettings().getUsername();
      m_operationData = createOperationData( m_session, waterBody, state, username );

      return Status.OK_STATUS;
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();

      final IStatus status = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, Messages.getString("CheckinStatePrepareOperation_1"), e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
  }

  /**
   * Either create a new, un-attached instance, or use the one existing in the database.
   */
  private State createOrUpdateState( final State existingState )
  {
    final State templateState = m_data.getState();

    /* copy some data from existing state that is not kept in wspm */
    if( existingState != null )
    {
      /* Copy those attributes that could have been edited by the user in the wizard */
      existingState.setDescription( templateState.getDescription() );
      existingState.setMeasurementDate( templateState.getMeasurementDate() );
      existingState.setSource( templateState.getSource() );

      return existingState;
    }

    return new State( templateState );
  }

  private CheckinStateOperationData createOperationData( final Session session, final WaterBody waterBody, final State state, final String username ) throws PdbConnectException
  {
    final TuhhReach reach = m_data.getReach();
    final IProfileFeature[] profiles = findProfiles( reach );

    final GafCodes gafCodes = new GafCodes();

    final String dbSrs = m_data.getDatabaseSrs();
    final ICoefficients coefficients = m_data.getCoefficients();
    final URI documentBase = m_data.getDocumentBase();

    final CrossSectionPartTypes partTypes = new CrossSectionPartTypes( session );

    return new CheckinStateOperationData( partTypes, gafCodes, coefficients, waterBody, state, reach, profiles, dbSrs, documentBase, username );
  }

  public void dispose( )
  {
    PdbUtils.closeSessionQuietly( m_session );
  }

  public boolean warnUser( final Shell shell, final String windowTitle )
  {
    if( m_existingStateZero == null )
    {
      /* new state, no need for silly questions */
      return true;
    }

    if( m_existingStateZero == ZeroState.T )
    {
      /* existing zero is write protected -> no operation! */
      MessageDialog.openError( shell, windowTitle, CheckinMessages.STR_STATE_ISZERO );
      return false;
    }
    else
    {
      /* existing state will be overwritten */
      return MessageDialog.openConfirm( shell, windowTitle, CheckinMessages.STR_STATE_WILL_BE_OVERWRITTEN );
    }
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

  public CheckinStateOperation createCheckinOperation( )
  {
    // REMARK: we want to keep the original gaf records; the real data of the culverts is in the metadata
    final boolean updateCulvertObjects = false;

    final CheckinStatePdbOperation pdbOperation = new CheckinStatePdbOperation( m_operationData, true, updateCulvertObjects );

    return new CheckinStateOperation( m_session, m_data, pdbOperation );
  }
}