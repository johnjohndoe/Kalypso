/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.risk.model.services;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * @author Dejan Antanaskovic
 */
public class LanduseStyleUpdateListener implements IResourceChangeListener
{
  private LanduseStyleUpdateService m_jobStyleUpdate;

  /**
   * @see org.eclipse.core.resources.IResourceChangeListener#resourceChanged(org.eclipse.core.resources.IResourceChangeEvent)
   */
  @Override
  public void resourceChanged( final IResourceChangeEvent event )
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    final IFolder scenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
    // if no scenario is active, just return
    if( scenarioFolder == null )
      return;

    final IPath resourcePath = scenarioFolder.getProjectRelativePath().append( "/models/RasterizationControlModel.gml" ); //$NON-NLS-1$

    final IResourceDelta rootDelta = event.getDelta();
    if( rootDelta == null )
      return;
    final IPath roughnessPath = scenarioFolder.getProject().getFullPath().append( resourcePath );
    final IResourceDelta fileDelta = rootDelta.findMember( roughnessPath );
    // TODO: check if it is a 1d2d project, there may be other projects with the same file

    if( fileDelta != null )
    {
      if( (fileDelta.getFlags() & IResourceDelta.CONTENT) != 0 )
        startStyleUpdateJob( (IFile) fileDelta.getResource() );
    }
  }

  public void startStyleUpdateJob( final IFile gmlDatabaseFile )
  {
    if( m_jobStyleUpdate != null )
      m_jobStyleUpdate.cancel();

    m_jobStyleUpdate = new LanduseStyleUpdateService( gmlDatabaseFile );

    // m_job.setSystem( true );
    m_jobStyleUpdate.setUser( false );
    m_jobStyleUpdate.setPriority( Job.LONG );
    m_jobStyleUpdate.setRule( m_jobStyleUpdate.getPolygonSymbolzerSldFile().getProject() );
    m_jobStyleUpdate.schedule( 500 );
  }

}
