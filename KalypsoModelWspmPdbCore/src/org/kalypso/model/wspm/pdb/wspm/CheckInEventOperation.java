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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.core.gml.WspmFixation;
import org.kalypso.model.wspm.pdb.connect.Executor;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.internal.wspm.CheckinEventPdbOperation;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class CheckInEventOperation implements ICoreRunnableWithProgress
{
  static final String STR_FAILED_TO_WRITE_TO_DATABASE = Messages.getString( "CheckInEventOperation.0" ); //$NON-NLS-1$

  private final CheckInEventData<WspmFixation> m_data;

  public CheckInEventOperation( final CheckInEventData<WspmFixation> data )
  {
    m_data = data;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    monitor.beginTask( Messages.getString( "CheckInEventOperation.1" ), 100 ); //$NON-NLS-1$

    Session session = null;

    try
    {
      final IPdbConnection connection = m_data.getConnection();
      session = connection.openSession();

      final Event event = m_data.getEvent();

      final WspmFixation fixation = m_data.getWspmObject();

      final Map<String, WaterBody> waterHash = m_data.getWaterHash();

      final CheckinEventPdbOperation operation = new CheckinEventPdbOperation( connection, waterHash, event, fixation, new SubProgressMonitor( monitor, 90 ) );
      new Executor( session, operation ).execute();

      session.close();

      updateFixation( event );

      return operation.getLog();
    }
    catch( final HibernateException e )
    {
      e.printStackTrace();
      return new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, STR_FAILED_TO_WRITE_TO_DATABASE, e );
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
      return new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, STR_FAILED_TO_WRITE_TO_DATABASE, e );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, Messages.STR_OPERATION_FAILED, e );
    }
    finally
    {
      monitor.done();
    }
  }

  /**
   * Some properties of the fixation may have changed in the wizard. We update the local reach with these properties.
   */
  private void updateFixation( final Event event ) throws Exception
  {
    final String name = event.getName();
    final String description = event.getDescription();

    final WspmFixation fixation = m_data.getWspmObject();

    final FeatureChange nameChange = new FeatureChange( fixation, Feature.QN_NAME, new ArrayList<>( Collections.singletonList( name ) ) );
    final FeatureChange descChange = new FeatureChange( fixation, Feature.QN_DESCRIPTION, description );

    final CommandableWorkspace workspace = m_data.getWorkspace();
    final ChangeFeaturesCommand command = new ChangeFeaturesCommand( workspace, new FeatureChange[] { nameChange, descChange } );
    workspace.postCommand( command );
  }
}