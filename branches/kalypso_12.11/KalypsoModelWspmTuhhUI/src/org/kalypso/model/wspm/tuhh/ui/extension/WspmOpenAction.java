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
package org.kalypso.model.wspm.tuhh.ui.extension;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IPerspectiveRegistry;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.intro.IIntroManager;
import org.eclipse.ui.navigator.CommonNavigator;
import org.eclipse.ui.navigator.CommonViewer;
import org.eclipse.ui.views.navigator.ResourceNavigator;
import org.kalypso.model.wspm.ui.product.WspmPerspectiveFactory;
import org.kalypso.module.welcome.actions.AbstractModuleProjectOpenAction;

/**
 * @author Dirk Kuch
 */
public class WspmOpenAction extends AbstractModuleProjectOpenAction
{
  public WspmOpenAction( )
  {
    super( KalypsoWspmTuhhModule.ID );
  }

  @Override
  protected IStatus doOpen( final Shell shell, final Point mousePosition, final IWorkbenchPage page, final IProject project ) throws CoreException
  {
    openPerspective( page );

    revealProjectInExplorer( page, project );

    final IFile iFile = project.getFile( "WSPM.gmv" ); //$NON-NLS-1$
    IDE.openEditor( page, iFile );
    return Status.OK_STATUS;
  }

  private void openPerspective( final IWorkbenchPage page )
  {
    /* hide intro */
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IIntroManager introManager = workbench.getIntroManager();
    introManager.closeIntro( introManager.getIntro() );

    /* Open desired perspective */
    if( page == null )
      return;

    final IPerspectiveRegistry perspectiveRegistry = workbench.getPerspectiveRegistry();

    final IPerspectiveDescriptor descriptor = perspectiveRegistry.findPerspectiveWithId( WspmPerspectiveFactory.ID );
    if( descriptor != null )
      page.setPerspective( descriptor );
  }

  private void revealProjectInExplorer( final IWorkbenchPage page, final IProject project ) throws PartInitException
  {
    // At least show project in Resource Navigator
    final StructuredSelection projectSelection = new StructuredSelection( project );

    final CommonNavigator projectExplorer = (CommonNavigator)page.findView( IPageLayout.ID_PROJECT_EXPLORER );
    if( projectExplorer != null )
    {
      page.showView( IPageLayout.ID_PROJECT_EXPLORER, null, IWorkbenchPage.VIEW_ACTIVATE );
      final CommonViewer commonViewer = projectExplorer.getCommonViewer();
      commonViewer.collapseAll();
      commonViewer.setSelection( projectSelection );
      commonViewer.expandToLevel( project, 1 );
    }
    else
    {
      final ResourceNavigator view = (ResourceNavigator)page.showView( IPageLayout.ID_RES_NAV );
      if( view != null )
      {
        final TreeViewer treeViewer = view.getTreeViewer();
        treeViewer.collapseAll();
        view.selectReveal( projectSelection );
        treeViewer.expandToLevel( project, 1 );
      }
    }
  }
}