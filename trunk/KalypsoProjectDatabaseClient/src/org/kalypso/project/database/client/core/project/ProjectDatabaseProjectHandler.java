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
package org.kalypso.project.database.client.core.project;

import java.util.Map;
import java.util.TreeMap;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.project.database.client.core.project.workspace.LocalWorkspaceProjectHandler;
import org.kalypso.project.database.client.core.project.workspace.RemoteWorkspaceProjectHandler;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;

/**
 * @author Dirk Kuch
 */
public class ProjectDatabaseProjectHandler implements ICoreRunnableWithProgress
{
  private final String[] m_local;

  private final String[] m_remote;

  private ProjectWrapper[] m_projects = null;

  /**
   * @param localIds
   *          handle project with nature id [x, y, z]
   * @param remoteIds
   *          handle remote projects with type id [x, y, z]
   */
  public ProjectDatabaseProjectHandler( final String[] localIds, final String[] remoteIds )
  {
    m_local = localIds;
    m_remote = remoteIds;

  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    final LocalWorkspaceProjectHandler local = new LocalWorkspaceProjectHandler( m_local );
    local.execute( monitor );

    final RemoteWorkspaceProjectHandler remote = new RemoteWorkspaceProjectHandler( m_remote );
    remote.execute( monitor );

    /* merge local and remote projects to one list of projects */
    final Map<String, ProjectWrapper> projects = new TreeMap<String, ProjectWrapper>();

    /* proces remote projects first, because local projects overwrites projects in previous defined map! */
    for( final KalypsoProjectBean bean : remote.getBeans() )
    {
      final String name = bean.getName();
      final ProjectWrapper handler = new ProjectWrapper( bean );

      projects.put( name, handler );
    }

    for( final IProject project : local.getProjects() )
    {
      final String name = project.getName();
      final ProjectWrapper handler = new ProjectWrapper( project );

      projects.put( name, handler );
    }

    m_projects = projects.values().toArray( new ProjectWrapper[] {} );

    return Status.OK_STATUS;
  }

  public ProjectWrapper[] getProjects( )
  {

    return m_projects;
  }
}
