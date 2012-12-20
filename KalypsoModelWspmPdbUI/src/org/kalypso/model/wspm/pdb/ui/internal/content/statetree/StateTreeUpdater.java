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
package org.kalypso.model.wspm.pdb.ui.internal.content.statetree;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.content.PdbLabelProvider;

/**
 * Helper that updates the state tree from the selection in the water body tree.
 *
 * @author Gernot Belger
 */
public class StateTreeUpdater
{
  private static final Object[] INPUT_NO_SELECTION = new Object[] { new Status( IStatus.INFO, WspmPdbUiPlugin.PLUGIN_ID, "No water body selected" ) };

  private StateInputUpdateJob m_updatejob;

  private final IPdbConnection m_connection;

  private final StateTreeComposite m_stateViewer;

  public StateTreeUpdater( final IPdbConnection connection, final StateTreeComposite stateViewer )
  {
    m_connection = connection;
    m_stateViewer = stateViewer;
  }

  /**
   * Set the selection of the water body tree in order to update the state tree.
   */
  public void setSelection( final IStructuredSelection selection )
  {

    if( m_updatejob != null )
    {
      m_updatejob.cancel();
      m_updatejob = null;
    }

    final WaterBody waterBody = waterBodyFromSelection( selection );
    if( waterBody == null )
    {
      m_stateViewer.setInput( INPUT_NO_SELECTION );
      return;
    }

    /* start loading */
    m_stateViewer.setInput( new Object[] { PdbLabelProvider.PENDING } );

    final StateInputUpdateJob updatejob = new StateInputUpdateJob( m_connection, waterBody.getId() );
    updatejob.addJobChangeListener( new JobChangeAdapter()
    {
      @Override
      public void done( final org.eclipse.core.runtime.jobs.IJobChangeEvent event )
      {
        final Object[] input = updatejob.getInput();
        // REMARK: null happens, if job was meanwhile cancelled
        if( input != null )
          handleStateViewerUpdate( input );
      }
    } );

    updatejob.schedule( 100 );

    m_updatejob = updatejob;
  }

  private WaterBody waterBodyFromSelection( final IStructuredSelection selection )
  {
    final Object firstElement = selection.getFirstElement();
    if( firstElement instanceof WaterBody )
      return (WaterBody)firstElement;

    return null;
  }

  protected void handleStateViewerUpdate( final Object[] input )
  {
    m_stateViewer.setInput( input );
  }
}