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
package org.kalypso.project.database.client.core.model.local;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.project.database.client.KalypsoProjectDatabaseClient;
import org.kalypso.project.database.client.core.model.interfaces.ILocalProject;
import org.kalypso.project.database.common.nature.IRemoteProjectPreferences;

/**
 * @author Dirk Kuch
 */
public class WorkspaceResourceManager
{
  private static WorkspaceResourceManager m_manager = null;

  private final Set<ILocalProject> m_projects = new HashSet<ILocalProject>();

  private WorkspaceResourceManager( )
  {
    final IResourceChangeListener changeListener = new IResourceChangeListener()
    {
      @Override
      public void resourceChanged( final IResourceChangeEvent event )
      {
        if( IResourceChangeEvent.POST_CHANGE != event.getType() )
          return;

        final IResourceDelta delta = event.getDelta();
        final IResourceDelta[] children = delta.getAffectedChildren();
        for( final IResourceDelta child : children )
        {
          final IResource resource = child.getResource();
          if( !(resource instanceof IProject) )
          {
            continue;
          }

          if( !containsRelevantChanges( child ) )
          {
            continue;
          }

          final IProject p = (IProject) resource;
          // FIXME: this is still aplied to projects that have nothing to do with the project database!
          final ILocalProject[] projects = getProjects();
          for( final ILocalProject project : projects )
          {
            if( project.equals( p ) )
            {
              try
              {
                try
                {
                  // FIXME: with this call, every(!) project automatically gets the remote-nature...
                  // this is not allowed!
                  final IRemoteProjectPreferences remotePreferences = project.getRemotePreferences();
                  if( remotePreferences != null )
                  {
                    remotePreferences.setModified( true );
                  }
                  break;
                }
                catch( final IllegalStateException e )
                {
                }
              }
              catch( final CoreException e )
              {
                KalypsoProjectDatabaseClient.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
              }
            }
          }
        }
      }
    };

    ResourcesPlugin.getWorkspace().addResourceChangeListener( changeListener );
  }

  public static WorkspaceResourceManager getInstance( )
  {
    if( m_manager == null )
    {
      m_manager = new WorkspaceResourceManager();
    }

    return m_manager;
  }

  protected boolean containsRelevantChanges( final IResourceDelta delta )
  {
    final IResourceDelta[] children = delta.getAffectedChildren();
    for( final IResourceDelta child : children )
    {
      final IResource resource = child.getResource();
      final String name = resource.getName();
      if( !".settings".equalsIgnoreCase( name ) )
        return true;
    }
    return false;
  }

  protected ILocalProject[] getProjects( )
  {
    return m_projects.toArray( new ILocalProject[] {} );
  }

  public void add( final LocalProjectHandler handler )
  {
    m_projects.add( handler );
  }

  public void remove( final LocalProjectHandler handler )
  {
    m_projects.remove( handler );
  }
}
