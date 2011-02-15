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
package org.kalypso.kalypsomodel1d2d.services;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.jobs.Job;

/**
 * @author Dejan Antanaskovic
 * 
 */
public class RoughnessStyleUpdateListener implements IResourceChangeListener
{
  public static final IPath ROUGHNESS_DATABASE_PATH = new Path( ".metadata/roughness.gml" ); //$NON-NLS-1$

  private RoughnessStyleUpdateService m_jobStyleUpdate;

  /**
   * @see org.eclipse.core.resources.IResourceChangeListener#resourceChanged(org.eclipse.core.resources.IResourceChangeEvent)
   */
  @Override
  public void resourceChanged( IResourceChangeEvent event )
  {
    final IResourceDelta rootDelta = event.getDelta();
    if( rootDelta == null )
      return;

    final IProject[] projects = ResourcesPlugin.getWorkspace().getRoot().getProjects();
    for( IProject project : projects )
    {
      final IPath roughnessPath = project.getFullPath().append( ROUGHNESS_DATABASE_PATH );
      final IResourceDelta fileDelta = rootDelta.findMember( roughnessPath );
      if( fileDelta != null )
      {
        if( (fileDelta.getFlags() & IResourceDelta.CONTENT) != 0 )
          startStyleUpdateJob( (IFile) fileDelta.getResource() );
      }
    }

  }

  public void startStyleUpdateJob( final IFile roughnessDBfile )
  {
    if( m_jobStyleUpdate != null )
      m_jobStyleUpdate.cancel();

    m_jobStyleUpdate = new RoughnessStyleUpdateService( roughnessDBfile );

// m_job.setSystem( true );
    m_jobStyleUpdate.setUser( false );
    m_jobStyleUpdate.setPriority( Job.LONG );
    m_jobStyleUpdate.setRule( m_jobStyleUpdate.getSldFile().getProject() );
    m_jobStyleUpdate.schedule( 500 );
  }

}
