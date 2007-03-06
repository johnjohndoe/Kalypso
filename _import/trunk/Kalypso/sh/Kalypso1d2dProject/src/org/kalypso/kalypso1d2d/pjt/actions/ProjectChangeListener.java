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
package org.kalypso.kalypso1d2d.pjt.actions;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.WorkbenchException;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypso1d2d.pjt.IActiveContextChangeListener;
import org.kalypso.kalypso1d2d.pjt.perspective.Perspective;
import org.kalypso.kalypso1d2d.pjt.views.SimulationModelDBView;
import org.kalypso.kalypso1d2d.pjt.views.WorkflowView;
import org.kalypso.scenarios.Scenario;

/**
 * @author Stefan Kurzbach
 */
public class ProjectChangeListener implements IActiveContextChangeListener
{
  private IProject m_newProject;

  private Scenario m_scenario;

  /**
   * @see org.kalypso.kalypso1d2d.pjt.IActiveContextChangeListener#activeProjectChanged(org.eclipse.core.resources.IProject)
   */
  public void activeContextChanged( final IProject newProject, final Scenario scenario )
  {
    if( newProject != m_newProject || scenario != m_scenario )
    {
      final UIJob job = new UIJob( "Changing work context..." )
      {
        @Override
        public IStatus runInUIThread( IProgressMonitor monitor )
        {
          final IWorkbench workbench = PlatformUI.getWorkbench();
          final IWorkbenchWindow activeWorkbenchWindow = workbench.getActiveWorkbenchWindow();
          final IWorkbenchPage workbenchPage = activeWorkbenchWindow.getActivePage();
          try
          {
            workbench.showPerspective( Perspective.ID, activeWorkbenchWindow );
          }
          catch( final WorkbenchException e )
          {
            return StatusUtilities.statusFromThrowable( e );
          }
          final IViewReference[] viewReferences = workbenchPage.getViewReferences();
          for( final IViewReference reference : viewReferences )
          {
            if( !shouldKeepView( reference ) )
            {
              workbenchPage.hideView( reference );
            }
          }
          return Status.OK_STATUS;
        }

        private boolean shouldKeepView( final IViewReference reference )
        {
          final String viewId = reference.getId();
          if( WorkflowView.ID.equals( viewId ) )
          {
            return true;
          }
          else if( SimulationModelDBView.ID.equals( viewId ) )
          {
            return true;
          }
          else if( reference.getPartName().equals( "Welcome" ) )
          {
            return true;
          }
          else
          {
            return false;
          }
        }
      };
      job.schedule();
      m_newProject = newProject;
      m_scenario = scenario;
    }
  }

}
