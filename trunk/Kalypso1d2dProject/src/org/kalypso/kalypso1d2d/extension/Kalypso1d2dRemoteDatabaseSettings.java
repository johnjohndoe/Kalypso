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
package org.kalypso.kalypso1d2d.extension;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypso1d2d.pjt.Kalypso1D2DProjectNature;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.project.database.client.extension.database.IKalypsoModuleDatabaseSettings;
import org.kalypso.project.database.client.extension.database.IProjectDatabaseFilter;
import org.kalypso.project.database.client.extension.database.handlers.ILocalProject;
import org.kalypso.project.database.client.extension.database.handlers.IProjectHandler;
import org.kalypso.project.database.client.extension.database.handlers.IRemoteProject;

/**
 * @author Dirk Kuch
 */
public class Kalypso1d2dRemoteDatabaseSettings implements IKalypsoModuleDatabaseSettings
{
  /**
   * @see org.kalypso.project.database.client.extension.database.IKalypsoRemoteDatabaseSettings#getModuleCommitType()
   */
  @Override
  public String getModuleCommitType( )
  {
    return Kalypso1d2dModule.ID;
  }

  /**
   * @see org.kalypso.project.database.client.extension.database.IKalypsoRemoteDatabaseSettings#getFilter()
   */
  @Override
  public IProjectDatabaseFilter getFilter( )
  {
    return new IProjectDatabaseFilter()
    {
      @Override
      public boolean select( final IProjectHandler handler )
      {
        if( handler instanceof ILocalProject )
        {
          try
          {
            final ILocalProject local = (ILocalProject) handler;
            return Kalypso1D2DProjectNature.isOfThisNature( local.getProject() );
          }
          catch( final CoreException e )
          {
            Kalypso1d2dProjectPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
          }
        }
        else if( handler instanceof IRemoteProject )
        {
          final IRemoteProject remote = (IRemoteProject) handler;
          final String projectType = remote.getBean().getProjectType();
          if( getModuleCommitType().equals( projectType ) )
            return true;
        }

        return false;
      }
    };
  }

  /**
   * @see org.kalypso.project.database.client.extension.database.IKalypsoModuleDatabaseSettings#hasManagedDirtyState()
   */
  @Override
  public boolean hasManagedDirtyState( )
  {
    return false;
  }
}
