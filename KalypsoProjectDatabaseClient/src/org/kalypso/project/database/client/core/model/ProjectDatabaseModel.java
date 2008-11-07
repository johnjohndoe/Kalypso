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
package org.kalypso.project.database.client.core.model;

import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.kalypso.project.database.client.core.interfaces.IProjectDatabaseFilter;
import org.kalypso.project.database.client.core.interfaces.IProjectDatabaseListener;
import org.kalypso.project.database.client.core.model.local.ILocalProject;
import org.kalypso.project.database.client.core.model.local.ILocalWorkspaceListener;
import org.kalypso.project.database.client.core.model.local.LocalWorkspaceModel;
import org.kalypso.project.database.client.core.model.remote.IRemoteProjectsListener;
import org.kalypso.project.database.client.core.model.remote.RemoteWorkspaceModel;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;

/**
 * @author Dirk Kuch
 */
public class ProjectDatabaseModel implements IProjectDatabaseModel, ILocalWorkspaceListener, IRemoteProjectsListener
{
  private LocalWorkspaceModel m_local;

  private RemoteWorkspaceModel m_remote;

  private Set<ProjectHandler> m_projects = null;

  private final Set<IProjectDatabaseListener> m_listener = new LinkedHashSet<IProjectDatabaseListener>();

  /**
   * @param localIds
   *          handle project with nature id [x, y, z]
   */
  public ProjectDatabaseModel( )
  {

    init();
  }

  private void init( )
  {
    m_local = new LocalWorkspaceModel();
    m_local.addListener( this );

    m_remote = new RemoteWorkspaceModel();
    m_remote.addListener( this );
  }

  public void dispose( )
  {
    m_local.dispose();
    m_remote.dispose();
  }

  private void buildProjectList( )
  {
    m_projects = new TreeSet<ProjectHandler>();

    final Map<String, ProjectHandler> projects = new HashMap<String, ProjectHandler>();

    final ILocalProject[] local = m_local.getProjects();
    final KalypsoProjectBean[] remote = m_remote.getBeans();

    for( final KalypsoProjectBean bean : remote )
    {
      final ProjectHandler handler = new ProjectHandler();
      handler.setBean( bean );
      projects.put( bean.getUnixName(), handler );
    }

    for( final ILocalProject project : local )
    {
      ProjectHandler handler = projects.get( project.getProject().getName() );
      if( handler == null )
      {
        handler = new ProjectHandler();
      }

      handler.setProject( project );

      projects.put( project.getProject().getName(), handler );
    }

    final Collection<ProjectHandler> collection = projects.values();
    m_projects.addAll( collection );
  }

  public ProjectHandler[] getProjects( )
  {
    if( m_projects == null )
      buildProjectList();

    return m_projects.toArray( new ProjectHandler[] {} );
  }

  /**
   * @see org.kalypso.project.database.client.core.model.local.ILocalWorkspaceListener#localWorkspaceChanged()
   */
  @Override
  public void localWorkspaceChanged( )
  {
    buildProjectList();

    for( final IProjectDatabaseListener listener : m_listener )
    {
      listener.projectModelChanged();
    }
  }

  /**
   * @see org.kalypso.project.database.client.core.model.remote.IRemoteWorkspaceListener#remoteWorkspaceChanged()
   */
  @Override
  public void remoteWorkspaceChanged( )
  {
    buildProjectList();

    for( final IProjectDatabaseListener listener : m_listener )
    {
      listener.projectModelChanged();
    }
  }

  public void addListener( final IProjectDatabaseListener listener )
  {
    m_listener.add( listener );
  }

  public void addRemoteListener( final IRemoteProjectsListener listener )
  {
    m_remote.addListener( listener );
  }

  public void removeListener( final IProjectDatabaseListener listener )
  {
    m_listener.remove( listener );
  }

  public ProjectHandler[] getProjects( final IProjectDatabaseFilter filter )
  {
    final Set<ProjectHandler> myProjects = new TreeSet<ProjectHandler>();

    final ProjectHandler[] projects = getProjects();
    for( final ProjectHandler handler : projects )
    {
      if( filter.select( handler ) )
        myProjects.add( handler );
    }

    return myProjects.toArray( new ProjectHandler[] {} );
  }

  public void setRemoteProjectsDirty( )
  {
    m_remote.setDirty();
  }

  public boolean isRemoteWorkspaceConnected( )
  {
    return m_remote.isRemoteWorkspaceConnected();
  }

  /**
   * @see org.kalypso.project.database.client.core.model.remote.IRemoteProjectsListener#remoteConnectionChanged()
   */
  @Override
  public void remoteConnectionChanged( final boolean connectionState )
  {
    buildProjectList();

    for( final IProjectDatabaseListener listener : m_listener )
    {
      listener.projectModelChanged();
    }
  }
}
