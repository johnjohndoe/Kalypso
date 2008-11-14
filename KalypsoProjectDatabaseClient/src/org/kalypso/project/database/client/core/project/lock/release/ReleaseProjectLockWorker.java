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
package org.kalypso.project.database.client.core.project.lock.release;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.project.database.client.KalypsoProjectDatabaseClient;
import org.kalypso.project.database.common.model.ProjectHandler;
import org.kalypso.project.database.common.nature.IRemoteProjectPreferences;
import org.kalypso.project.database.sei.IProjectDatabase;

/**
 * Acquires a project lock (lock ticket) in the model base and update
 * {@link org.kalypso.project.database.common.nature.RemoteProjectNature} lock settings
 * 
 * @author Dirk Kuch
 */
public class ReleaseProjectLockWorker implements ICoreRunnableWithProgress
{

  private final ProjectHandler m_handler;

  public ReleaseProjectLockWorker( final ProjectHandler handler )
  {
    m_handler = handler;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    /* project preferences */
    if( !m_handler.isLocalRemoteProject() )
      throw new CoreException( StatusUtilities.createErrorStatus( String.format( "Resolving RemoteProjectNature of project \"%s\" failed.", m_handler.getName() ) ) );

    final IRemoteProjectPreferences preferences = m_handler.getRemotePreferences();
    final String ticket = preferences.getEditTicket();
    Assert.isNotNull( ticket );

    final IProjectDatabase service = KalypsoProjectDatabaseClient.getService();
    final Boolean released = service.releaseProjectEditLock( m_handler.getUnixName(), ticket );
    Assert.isTrue( released );

    /* reset edit ticket */
    preferences.setEditTicket( "" );
    Assert.isTrue( "".equals( preferences.getEditTicket().trim() ) );

    return Status.OK_STATUS;
  }
}
