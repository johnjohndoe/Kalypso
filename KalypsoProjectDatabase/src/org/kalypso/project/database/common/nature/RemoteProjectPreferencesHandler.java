/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.project.database.common.nature;

import org.eclipse.core.resources.WorkspaceJob;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;

/**
 * Remote project settings of {@link org.eclipse.core.resources.IProjectNature} -> {@link RemoteProjectNature}
 * 
 * @author Dirk Kuch
 */
public class RemoteProjectPreferencesHandler implements IRemoteProjectPreferences
{
  private static final String PROJECT_LOCK_TICKET = "project.lock";

  private static final String PROJECT_IS_ON_SERVER = "project.is.on.server";

  private static final String PROJECT_DOWNLOADED_VERSION = "project.downloaded.version";

  private static final String REMOTE_PROJECT_TYPE = "project.remote.type";

  protected final IEclipsePreferences m_node;

  public RemoteProjectPreferencesHandler( final IEclipsePreferences node )
  {
    m_node = node;
  }

  /**
   * @see org.kalypso.project.database.common.nature.IRemoteProjectPreferences#isLocked()
   */
  @Override
  public boolean isLocked( )
  {
    final String ticket = getEditTicket();
    if( ticket == null || "".equals( ticket.trim() ) )
    {
      return false;
    }

    return true;
  }

  /**
   * @see org.kalypso.project.database.common.nature.IRemoteProjectPreferences#setEditTicket(java.lang.String)
   */
  @Override
  public void setEditTicket( final String ticket )
  {
    m_node.put( PROJECT_LOCK_TICKET, ticket );
    flush();
  }

  /**
   * @see org.kalypso.project.database.common.nature.IRemoteProjectPreferences#getEditTicket()
   */
  @Override
  public String getEditTicket( )
  {
    return m_node.get( PROJECT_LOCK_TICKET, null );
  }

  /**
   * @see org.kalypso.project.database.common.nature.IRemoteProjectPreferences#isOnServer()
   */
  @Override
  public boolean isOnServer( )
  {
    return m_node.getBoolean( PROJECT_IS_ON_SERVER, false );
  }

  /**
   * @see org.kalypso.project.database.common.nature.IRemoteProjectPreferences#isOnServer()
   */
  @Override
  public Integer getVersion( )
  {
    return Integer.valueOf( m_node.get( PROJECT_DOWNLOADED_VERSION, "-1" ) );
  }

  /**
   * @see org.kalypso.project.database.common.nature.IRemoteProjectPreferences#setIsOnServer(boolean)
   */
  @Override
  public void setIsOnServer( final boolean onServer )
  {
    m_node.putBoolean( PROJECT_IS_ON_SERVER, Boolean.valueOf( onServer ) );
    flush();
  }

  private void flush( )
  {
    new WorkspaceJob( "" )
    {
      @Override
      public IStatus runInWorkspace( final IProgressMonitor monitor )
      {
        try
        {
          m_node.flush();
        }
        catch( final Exception e )
        {
        }
        return Status.OK_STATUS;
      }
    }.schedule();

  }

  /**
   * @see org.kalypso.project.database.common.nature.IRemoteProjectPreferences#setVersion(java.lang.Integer)
   */
  @Override
  public void setVersion( final Integer version )
  {
    m_node.put( PROJECT_DOWNLOADED_VERSION, version.toString() );
    flush();
  }

  /**
   * @see org.kalypso.project.database.common.nature.IRemoteProjectPreferences#getProjectType()
   */
  @Override
  public String getProjectType( )
  {
    return m_node.get( REMOTE_PROJECT_TYPE, "blub" );
  }

  /**
   * @see org.kalypso.project.database.common.nature.IRemoteProjectPreferences#setProjectType(java.lang.String)
   */
  @Override
  public void setProjectType( final String projectType )
  {
    m_node.put( REMOTE_PROJECT_TYPE, projectType );
    flush();
  }

}
