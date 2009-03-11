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
package org.kalypso.project.database.client.core;

import org.eclipse.core.resources.WorkspaceJob;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.project.database.client.KalypsoProjectDatabaseClient;
import org.kalypso.project.database.client.core.model.interfaces.ILocalProject;
import org.kalypso.project.database.client.core.model.interfaces.IProjectDatabaseModel;
import org.kalypso.project.database.client.core.model.interfaces.ITranscendenceProject;
import org.kalypso.project.database.client.core.project.commit.UpdateProjectWorker;
import org.kalypso.project.database.client.core.project.create.CreateRemoteProjectWorker;
import org.kalypso.project.database.client.core.project.lock.acquire.AcquireProjectLockWorker;
import org.kalypso.project.database.client.core.project.lock.release.ReleaseProjectLockWorker;

/**
 * @author Dirk Kuch
 */
public class ProjectDataBaseController
{
  protected static WorkspaceJob JOB = null;

  public static IStatus createRemoteProject( final ILocalProject handler )
  {
    final CreateRemoteProjectWorker worker = new CreateRemoteProjectWorker( handler );
    final IStatus status = ProgressUtilities.busyCursorWhile( worker );
    setDirty();

    return status;
  }

  /**
   * set project database model dirty
   */
  synchronized private static void setDirty( )
  {
    if( JOB == null )
    {
      JOB = new WorkspaceJob( "" ) //$NON-NLS-1$
      {

        @Override
        public IStatus runInWorkspace( final IProgressMonitor monitor )
        {
          final IProjectDatabaseModel model = KalypsoProjectDatabaseClient.getDefault().getProjectDatabaseModel();
          model.setRemoteProjectsDirty();

          JOB = null;

          return Status.OK_STATUS;
        }
      };

      JOB.schedule( 100 );
    }
    else
    {
      JOB.schedule( 100 );
    }

  }

  public static IStatus updateProject( final ITranscendenceProject handler )
  {
    final UpdateProjectWorker worker = new UpdateProjectWorker( handler );
    final IStatus status = ProgressUtilities.busyCursorWhile( worker );
    setDirty();

    return status;
  }

  public static IStatus releaseProjectLock( final ILocalProject handler )
  {
    final ReleaseProjectLockWorker worker = new ReleaseProjectLockWorker( handler );
    final IStatus status = ProgressUtilities.busyCursorWhile( worker );
    setDirty();

    return status;
  }

  public static IStatus acquireProjectLock( final ILocalProject handler )
  {
    final AcquireProjectLockWorker worker = new AcquireProjectLockWorker( handler );
    final IStatus status = ProgressUtilities.busyCursorWhile( worker );
    setDirty();

    return status;
  }

}
