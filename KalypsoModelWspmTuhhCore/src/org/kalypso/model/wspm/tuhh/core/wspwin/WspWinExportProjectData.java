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
package org.kalypso.model.wspm.tuhh.core.wspwin;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.databinding.observable.set.ISetChangeListener;
import org.eclipse.core.databinding.observable.set.SetChangeEvent;
import org.eclipse.core.databinding.observable.set.WritableSet;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.kalypso.model.wspm.tuhh.core.util.WspmTuhhUtils;

/**
 * @author Gernot Belger
 */
public class WspWinExportProjectData extends WspWinExportData
{
  private final WritableSet m_selectedProjectsSet = new WritableSet();

  private IProject[] m_selectedProjects = new IProject[0];

  public WspWinExportProjectData( )
  {
    m_selectedProjectsSet.addSetChangeListener( new ISetChangeListener()
    {
      @Override
      public void handleSetChange( final SetChangeEvent event )
      {
        projectsChanged();
      }
    } );
  }

  protected void projectsChanged( )
  {
    m_selectedProjects = (IProject[]) m_selectedProjectsSet.toArray( new IProject[m_selectedProjectsSet.size()] );
  }

  public void setSelection( final IStructuredSelection selection )
  {
    for( final Object element : selection.toArray() )
    {
      if( element instanceof IResource )
        m_selectedProjectsSet.add( ((IResource) element).getProject() );
    }
  }

  public WritableSet getSelectedProjectList( )
  {
    return m_selectedProjectsSet;
  }

  public IProject[] getSelectedProjects( )
  {
    return m_selectedProjects;
  }

  @Override
  public IProject[] getWspmProjects( )
  {
    final IWorkspace workspace = ResourcesPlugin.getWorkspace();
    final IProject[] projects = workspace.getRoot().getProjects();
    final Collection<IProject> wspmProjects = new ArrayList<>();

    for( final IProject project : projects )
    {
      if( WspmTuhhUtils.isWspmTuhhProject( project ) )
        wspmProjects.add( project );
    }

    return wspmProjects.toArray( new IProject[wspmProjects.size()] );
  }
}