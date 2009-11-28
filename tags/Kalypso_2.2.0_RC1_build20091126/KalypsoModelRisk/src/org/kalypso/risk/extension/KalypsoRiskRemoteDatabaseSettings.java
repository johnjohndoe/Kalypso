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
package org.kalypso.risk.extension;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.project.database.client.extension.database.IKalypsoModuleDatabaseSettings;
import org.kalypso.project.database.client.extension.database.IProjectDatabaseFilter;
import org.kalypso.project.database.client.extension.database.handlers.ILocalProject;
import org.kalypso.project.database.client.extension.database.handlers.IProjectHandler;
import org.kalypso.project.database.client.extension.database.handlers.IRemoteProject;
import org.kalypso.project.database.client.extension.project.IKalypsoModuleProjectOpenAction;
import org.kalypso.project.database.client.extension.project.SzenarioProjectOpenAction;
import org.kalypso.risk.plugin.KalypsoRiskPlugin;

import de.renew.workflow.base.IWorkflow;
import de.renew.workflow.connector.WorkflowProjectNature;

/**
 * @author Dirk Kuch
 */
public class KalypsoRiskRemoteDatabaseSettings implements IKalypsoModuleDatabaseSettings
{

  /**
   * @see org.kalypso.project.database.client.extension.database.IKalypsoRemoteDatabaseSettings#getModuleCommitType()
   */
  @Override
  public String getModuleCommitType( )
  {
    return KalypsoRiskModule.ID;
  }

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
            final WorkflowProjectNature nature = WorkflowProjectNature.toThisNature( local.getProject() );
            if( nature == null )
              return false;

            final IWorkflow workflow = nature.getCurrentWorklist();
            final String uri = workflow.getURI();

            return uri.contains( "http___www.tu-harburg.de_wb_kalypso_risk__WF_KalypsoRisk" ); //$NON-NLS-1$
          }
          catch( final CoreException e )
          {
            KalypsoRiskPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
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

  @Override
  public IKalypsoModuleProjectOpenAction getProjectOpenAction( )
  {
    return new SzenarioProjectOpenAction();
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
