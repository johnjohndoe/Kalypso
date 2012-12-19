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
package org.kalypso.model.wspm.pdb.ui.internal;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IPerspectiveListener;
import org.eclipse.ui.IStartup;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchListener;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PerspectiveAdapter;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.progress.IProgressService;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.wspm.pdb.ui.internal.content.PdbView;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.ui.internal.wspm.InitPdbDataOperation;
import org.kalypso.model.wspm.pdb.ui.internal.wspm.PdbWspmProject;
import org.kalypso.model.wspm.tuhh.ui.light.WspmLightPerspective;

/**
 * Initializes the WSPM project directly after startup.
 * 
 * @author Gernot Belger
 */
public class PdbStartup implements IStartup
{
  private final IWorkbenchListener m_workbenchListener = new IWorkbenchListener()
  {
    @Override
    public boolean preShutdown( final IWorkbench workbench, final boolean forced )
    {
      return handlePreShutdown();
    }

    @Override
    public void postShutdown( final IWorkbench workbench )
    {
    }
  };

  final IPerspectiveListener m_perspectiveListener = new PerspectiveAdapter()
  {
    @Override
    public void perspectiveActivated( final IWorkbenchPage page, final IPerspectiveDescriptor perspective )
    {
      handlePerspectiveActivated( page, perspective );
    }
  };

  @Override
  public void earlyStartup( )
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    workbench.addWorkbenchListener( m_workbenchListener );

    workbench.getDisplay().asyncExec( new Runnable()
    {
      @Override
      public void run( )
      {
        final IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();

        final IPerspectiveDescriptor perspective = window.getActivePage().getPerspective();
        final String id = perspective.getId();
        if( WspmLightPerspective.ID.equals( id ) )
          doStartup( window );
        else
          window.addPerspectiveListener( m_perspectiveListener );
      }
    } );
  }

  protected boolean handlePreShutdown( )
  {
    final PdbWspmProject wspmProject = WspmPdbUiPlugin.getDefault().getWspmProject();
    if( wspmProject == null )
      return true;

    return wspmProject.askForProjectSave();
  }

  protected void handlePerspectiveActivated( final IWorkbenchPage page, final IPerspectiveDescriptor perspective )
  {
    final String id = perspective.getId();
    if( WspmLightPerspective.ID.equals( id ) )
    {
      final IWorkbenchWindow window = page.getWorkbenchWindow();
      doStartup( window );
      /* Only do it once */
      window.removePerspectiveListener( m_perspectiveListener );
    }
  }

  protected void doStartup( final IWorkbenchWindow window )
  {
    final PdbWspmProject wspmProject = new PdbWspmProject( window );

    final InitPdbDataOperation operation = new InitPdbDataOperation( wspmProject );
    final IProgressService progressService = (IProgressService)window.getService( IProgressService.class );
    final IStatus status = ProgressUtilities.busyCursorWhile( progressService, operation, null );

    if( !status.isOK() )
      StatusDialog.open( window.getShell(), status, Messages.getString( "PdbStartup.0" ) ); //$NON-NLS-1$

    if( status.matches( IStatus.ERROR ) )
    {
      PlatformUI.getWorkbench().close();
      return;
    }

    if( status.matches( IStatus.ERROR ) )
    {
      PlatformUI.getWorkbench().close();
      return;
    }

    WspmPdbUiPlugin.getDefault().setWspmProject( wspmProject );

    PdbView.updateView( window );
  }
}