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

import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Set;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.afgui.extension.IProjectDatabaseFilter;
import org.kalypso.afgui.extension.IProjectHandler;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.project.database.client.core.model.interfaces.ILocalProject;
import org.kalypso.project.database.client.core.model.interfaces.IProjectDatabaseModel;
import org.kalypso.project.database.client.core.model.interfaces.IRemoteProject;
import org.kalypso.project.database.client.core.model.local.ILocalWorkspaceListener;
import org.kalypso.project.database.client.core.model.local.LocalWorkspaceModel;
import org.kalypso.project.database.client.core.model.remote.IRemoteProjectsListener;
import org.kalypso.project.database.client.core.model.remote.RemoteProjectHandler;
import org.kalypso.project.database.client.core.model.remote.RemoteWorkspaceModel;
import org.kalypso.project.database.client.core.model.transcendence.TranscendenceProjectHandler;
import org.kalypso.project.database.client.core.utils.ProjectDatabaseServerUtils;
import org.kalypso.project.database.common.interfaces.IProjectDatabaseListener;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;

/**
 * @author Dirk Kuch
 */
public class ProjectDatabaseModel implements IProjectDatabaseModel, ILocalWorkspaceListener, IRemoteProjectsListener
{
  private LocalWorkspaceModel m_local;

  private RemoteWorkspaceModel m_remote = null;

  private Set<IProjectHandler> m_projects = null;

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

    if( ProjectDatabaseServerUtils.handleRemoteProject() )
    {
      m_remote = new RemoteWorkspaceModel();
      m_remote.addListener( this );
    }

  }

  public void dispose( )
  {
    m_local.dispose();

    if( m_remote != null )
    {
      m_remote.dispose();
    }
  }

  private void buildProjectList( )
  {
    m_projects = new HashSet<IProjectHandler>();

    final ILocalProject[] local = m_local.getProjects();
    IRemoteProject[] remote = new IRemoteProject[] {};

    if( m_remote != null )
    {
      final Set<IRemoteProject> handler = new HashSet<IRemoteProject>();

      final KalypsoProjectBean[] beans = m_remote.getBeans();
      for( final KalypsoProjectBean bean : beans )
      {
        handler.add( new RemoteProjectHandler( bean ) );
      }

      remote = handler.toArray( new IRemoteProject[] {} );
    }

    for( final ILocalProject handler : local )
    {
      if( ArrayUtils.contains( remote, handler ) )
      {
        final int index = ArrayUtils.indexOf( remote, handler );

        final IRemoteProject r = remote[index];
        remote = (IRemoteProject[]) ArrayUtils.remove( remote, index );

        m_projects.add( new TranscendenceProjectHandler( handler, r ) );
      }
      else
      {
        m_projects.add( handler );
      }
    }

    for( final IRemoteProject r : remote )
    {
      m_projects.add( r );
    }

// /* clean up */
// final Collection<AbstractProjectHandler> collection = projects.values();
// for( final AbstractProjectHandler handler : collection )
// {

// if( handler.isLocal() && handler.isLocalRemoteProject() )
// {
// /* reset false remote preferences */
// final IRemoteProjectPreferences preferences = handler.getRemotePreferences();
//
// if( preferences.isOnServer() && !handler.isRemote() )
// {
// preferences.setIsOnServer( false );
// }
//
// if( !preferences.isOnServer() && handler.isRemote() )
// {
// preferences.setIsOnServer( true );
// }
// }

  }

  public IProjectHandler[] getProjects( )
  {
    if( m_projects == null )
    {
      buildProjectList();
    }

    return m_projects.toArray( new IProjectHandler[] {} );
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
    if( m_remote != null )
    {
      m_remote.addListener( listener );
    }
  }

  public void removeListener( final IProjectDatabaseListener listener )
  {
    m_listener.remove( listener );
  }

  public IProjectHandler[] getProjects( final IProjectDatabaseFilter filter )
  {
    final Set<IProjectHandler> myProjects = new HashSet<IProjectHandler>();

    final IProjectHandler[] projects = getProjects();
    for( final IProjectHandler handler : projects )
    {
      if( filter.select( handler ) )
      {
        myProjects.add( handler );
      }
    }

    return myProjects.toArray( new AbstractProjectHandler[] {} );
  }

  public void setRemoteProjectsDirty( )
  {
    if( m_remote != null )
    {
      m_remote.setDirty();
    }
  }

  public IStatus getRemoteConnectionState( )
  {
    if( m_remote != null )
    {
      return m_remote.getRemoteConnectionState();
    }

    return StatusUtilities.createWarningStatus( "Laufzeiteinstellung verhindert Behandlung von Remote-Projekten" );
  }

  /**
   * @see org.kalypso.project.database.client.core.model.remote.IRemoteProjectsListener#remoteConnectionChanged()
   */
  @Override
  public void remoteConnectionChanged( final IStatus connectionState )
  {
    buildProjectList();

    for( final IProjectDatabaseListener listener : m_listener )
    {
      listener.projectModelChanged();
    }
  }
}
