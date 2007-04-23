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
package org.kalypso.kalypso1d2d.pjt.actions;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IPerspectiveListener;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.contexts.IContextService;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.afgui.scenarios.Scenario;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.partlistener.PartAdapter;
import org.kalypso.kalypso1d2d.pjt.IActiveContextChangeListener;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypso1d2d.pjt.perspective.Perspective;
import org.kalypso.kalypso1d2d.pjt.views.WorkflowView;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.MapModellContextSwitcher;
import org.kalypso.ogc.gml.outline.GisMapOutlineView;
import org.kalypso.ui.editor.mapeditor.AbstractMapPart;

/**
 * @author Stefan Kurzbach
 */
public class PerspectiveWatcher extends PartAdapter implements IActiveContextChangeListener, IPartListener, IPerspectiveListener
{
  private IProject m_currentProject;

  private Scenario m_currentScenario;

  private final MapModellContextSwitcher m_mapModellContextSwitcher = new MapModellContextSwitcher();

  /**
   * @see org.kalypso.kalypso1d2d.pjt.IActiveContextChangeListener#activeProjectChanged(org.eclipse.core.resources.IProject)
   */
  public void activeContextChanged( final IProject newProject, final Scenario scenario )
  {
    if( newProject != m_currentProject || scenario != m_currentScenario )
    {
      final IWorkbench workbench = PlatformUI.getWorkbench();
      if( workbench.isClosing() )
        return;

      final UIJob job = new UIJob( "Perspektive öffnen" )
      {
        @Override
        public IStatus runInUIThread( final IProgressMonitor monitor )
        {
          final IWorkbenchWindow activeWorkbenchWindow = workbench.getActiveWorkbenchWindow();
          final IWorkbenchPage workbenchPage = activeWorkbenchWindow.getActivePage();
          try
          {
            workbench.showPerspective( Perspective.ID, activeWorkbenchWindow );
            cleanPerspective( workbenchPage );
          }
          catch( final Throwable e )
          {
            final IStatus status = StatusUtilities.statusFromThrowable( e );
            Kalypso1d2dProjectPlugin.getDefault().getLog().log( status );
            ErrorDialog.openError( workbench.getDisplay().getActiveShell(), "Fehler beim Öffnen der Perspektive", "", status );
          }
          return Status.OK_STATUS;
        }

      };
      job.schedule();
      m_currentProject = newProject;
      m_currentScenario = scenario;
    }
  }

  public void cleanPerspective( final IWorkbenchPage workbenchPage )
  {
    final IViewReference[] viewReferences = workbenchPage.getViewReferences();
    for( final IViewReference reference : viewReferences )
    {
      if( !shouldKeepView( reference ) )
      {
        workbenchPage.hideView( reference );
      }
    }
  }

  /**
   * @see org.eclipse.ui.IPerspectiveListener#perspectiveActivated(org.eclipse.ui.IWorkbenchPage,
   *      org.eclipse.ui.IPerspectiveDescriptor)
   */
  public void perspectiveActivated( final IWorkbenchPage page, final IPerspectiveDescriptor perspective )
  {
    if( perspective.getId().equals( Perspective.ID ) )
    {
      page.addPartListener( this );
      // set focus to scenario view
      IViewPart scenarioView;
      try
      {
        scenarioView = page.showView( Perspective.SCENARIO_VIEW_ID );
        if( scenarioView != null )
        {
          page.activate( scenarioView );
        }
      }
      catch( final PartInitException e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        Kalypso1d2dProjectPlugin.getDefault().getLog().log( status );
        ErrorDialog.openError( page.getWorkbenchWindow().getShell(), "Fehler", "Fehler beim Öffnen der Szenarioansicht", status );
      }
    }
    else
    {
      page.removePartListener( this );
    }
  }

  /**
   * @see org.eclipse.ui.IPerspectiveListener#perspectiveChanged(org.eclipse.ui.IWorkbenchPage,
   *      org.eclipse.ui.IPerspectiveDescriptor, java.lang.String)
   */
  public void perspectiveChanged( final IWorkbenchPage page, final IPerspectiveDescriptor perspective, final String changeId )
  {
    // do nothing
  }

  /**
   * @see org.eclipse.ui.IPartListener#partClosed(org.eclipse.ui.IWorkbenchPart)
   */
  @Override
  public void partClosed( final IWorkbenchPart part )
  {
    // TODO consider (de)registering the mapModellContextSwitcher in the MapPanel itself
    if( part instanceof AbstractMapPart )
    {
      // deregister contextSwitcher on map panel
      final IContextService contextService = (IContextService) part.getSite().getService( IContextService.class );
      final MapPanel mapPanel = (MapPanel) part.getAdapter( MapPanel.class );
      mapPanel.removeModellListener( m_mapModellContextSwitcher );
      m_mapModellContextSwitcher.removeContextService( contextService );
      mapPanel.getWidgetManager().setActualWidget( null );
    }
  }

  /**
   * @see org.eclipse.ui.IPartListener#partOpened(org.eclipse.ui.IWorkbenchPart)
   */
  @Override
  public void partOpened( final IWorkbenchPart part )
  {
    if( part instanceof AbstractMapPart )
    {
      // register contextSwitcher on map panel
      final IContextService contextService = (IContextService) part.getSite().getService( IContextService.class );
      final MapPanel mapPanel = (MapPanel) part.getAdapter( MapPanel.class );
      mapPanel.addModellListener( m_mapModellContextSwitcher );
      m_mapModellContextSwitcher.addContextService( contextService );
    }
  }

  private boolean shouldKeepView( final IViewReference reference )
  {
    final String viewId = reference.getId();
    if( WorkflowView.ID.equals( viewId ) )
    {
      return true;
    }
    else if( Perspective.SCENARIO_VIEW_ID.equals( viewId ) )
    {
      return true;
    }
    else if( GisMapOutlineView.ID.equals( viewId ) )
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

  public void dispose( )
  {
    m_mapModellContextSwitcher.dispose();
  }
}
