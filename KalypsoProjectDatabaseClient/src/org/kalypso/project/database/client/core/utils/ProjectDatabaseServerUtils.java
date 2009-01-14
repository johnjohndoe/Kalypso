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
package org.kalypso.project.database.client.core.utils;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.project.database.client.IProjectDataBaseClientConstant;
import org.kalypso.project.database.client.KalypsoProjectDatabaseClient;
import org.kalypso.project.database.client.core.model.ProjectHandler;
import org.kalypso.project.database.common.nature.IRemoteProjectPreferences;
import org.kalypso.project.database.sei.IProjectDatabase;
import org.kalypso.project.database.sei.beans.KalypsoProjectBean;

/**
 * @author kuch
 */
public class ProjectDatabaseServerUtils
{
  public static boolean isServerOnline( )
  {
    try
    {
      final IProjectDatabase service = KalypsoProjectDatabaseClient.getServiceUnblocking();
      if( service == null )
      {
        return false;
      }

      service.ping();

      return true;
    }
    catch( final Exception e )
    {
      return false;
    }
  }

  public static boolean handleRemoteProject( )
  {
    final String property = System.getProperty( IProjectDataBaseClientConstant.HANDLE_REMOTE_PROJECTS );
    if( property != null && Boolean.valueOf( property ) == true )
    {
      return true;
    }

    return false;
  }

  public static boolean isUpdateAvailable( final ProjectHandler handler )
  {
    if( handler.isLocalRemoteProject() )
    {
      try
      {
        final IRemoteProjectPreferences preferences = handler.getRemotePreferences();
        final Integer localVersion = preferences.getVersion();

        final KalypsoProjectBean bean = handler.getBean();
        final Integer remoteVersion = bean.getProjectVersion();

        if( localVersion.intValue() < remoteVersion.intValue() )
        {
          return true;
          // happens while proejct update action
// else if( localVersion.intValue() > remoteVersion.intValue() )
// throw new IllegalStateException( "Should never happen: localVersion.intValue() > remoteVersion.intValue()" );
        }

      }
      catch( final CoreException e )
      {
        KalypsoProjectDatabaseClient.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
      }
    }

    return false;
  }
}
