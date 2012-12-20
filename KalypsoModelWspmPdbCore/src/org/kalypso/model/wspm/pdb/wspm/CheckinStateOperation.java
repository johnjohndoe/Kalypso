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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.pdb.connect.Executor;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class CheckinStateOperation implements ICoreRunnableWithProgress
{
  static final String STR_FAILED_TO_WRITE_TO_DATABASE = Messages.getString( "CheckInEventOperation.0" ); //$NON-NLS-1$

  private final CheckinStateData m_data;

  private final Session m_session;

  private final ICheckinStatePdbOperation m_operation;

  public CheckinStateOperation( final Session session, final CheckinStateData data, final ICheckinStatePdbOperation operation )
  {
    m_session = session;
    m_data = data;
    m_operation = operation;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( Messages.getString( "CheckinStateOperation.1" ), 100 ); //$NON-NLS-1$

    try
    {
      m_operation.setMonitor( new SubProgressMonitor( monitor, 90 ) );

      new Executor( m_session, m_operation ).execute();
      final IStatus status = m_operation.getStatus();

      /* write new state name etc. back into reach element, so it is still in sync with db */
      final State newState = m_data.getState();
      updateReach( newState );

      return status;
    }
    catch( final HibernateException e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, STR_FAILED_TO_WRITE_TO_DATABASE, e );
      throw new CoreException( status );
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, STR_FAILED_TO_WRITE_TO_DATABASE, e );
      throw new CoreException( status );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, Messages.STR_OPERATION_FAILED, e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
    finally
    {
      monitor.done();
    }
  }

  /**
   * Some properties of the state may have changed in the wizard. We update the local reach with these properties.
   */
  private void updateReach( final State state ) throws Exception
  {
    final String name = state.getName();
    final String description = state.getDescription();

    final TuhhReach reach = m_data.getReach();

    final FeatureChange nameChange = new FeatureChange( reach, Feature.QN_NAME, new ArrayList<>( Collections.singletonList( name ) ) );
    final FeatureChange descChange = new FeatureChange( reach, Feature.QN_DESCRIPTION, description );

    final CommandableWorkspace workspace = m_data.getWorkspace();
    final ChangeFeaturesCommand command = new ChangeFeaturesCommand( workspace, new FeatureChange[] { nameChange, descChange } );
    workspace.postCommand( command );
  }

  /** Creates the name for the cross section in the database. Uses profile name, or station if name is empty. */
  public static String createCrossSectionName( final String name, final BigDecimal station )
  {
    if( !StringUtils.isEmpty( name ) )
      return name;

    /* Fall back to station as name */
    if( station == null )
      return StringUtils.EMPTY;

    return String.format( "%.4f", station ); //$NON-NLS-1$
  }
}