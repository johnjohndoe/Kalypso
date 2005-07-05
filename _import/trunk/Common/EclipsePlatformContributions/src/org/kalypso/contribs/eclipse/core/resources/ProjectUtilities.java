/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.contribs.eclipse.core.resources;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

/**
 * @author belger
 */
public class ProjectUtilities
{
  private ProjectUtilities()
  {
  // do not instantiate
  }

  /**
   * Findet alle Projekte einer Selektion von Resourcen
   * 
   * @param selection
   * @return list of projects (not null)
   */
  public static IProject[] findProjectsFromSelection( final ISelection selection )
  {
    // gleiche Projekte sollen nur einen Eintrag gebens
    final Collection projects = new HashSet();
    if( selection != null && !selection.isEmpty() && selection instanceof IStructuredSelection )
    {
      final IStructuredSelection ssel = (IStructuredSelection)selection;
      for( final Iterator iter = ssel.iterator(); iter.hasNext(); )
      {
        final Object resource = iter.next();
        if( resource instanceof IResource )
          projects.add( ( (IResource)resource ).getProject() );
        else if( resource instanceof IAdaptable )
        {
          final IResource res = (IResource)( (IAdaptable)resource ).getAdapter( IResource.class );
          if( res != null )
            projects.add( res.getProject() );
        }
      }
    }

    return (IProject[])projects.toArray( new IProject[projects.size()] );
  }

  /**
   * TODO does this work? seems not... Note from Marc: this only works when the navigator has an active selection
   * 
   * Returns the currently selected project from the navigator.
   * 
   * @return list of selected projects
   */
  public static IProject[] getSelectedProjects()
  {
    final IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
    final ISelection selection = window.getSelectionService().getSelection( IPageLayout.ID_RES_NAV );

    final IProject[] projects = findProjectsFromSelection( selection );

    return projects;
  }
}