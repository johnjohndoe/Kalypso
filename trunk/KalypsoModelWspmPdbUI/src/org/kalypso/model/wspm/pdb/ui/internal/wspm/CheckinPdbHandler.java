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
package org.kalypso.model.wspm.pdb.ui.internal.wspm;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.handlers.HandlerUtil;
import org.kalypso.contribs.eclipse.core.commands.HandlerUtils;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.wspm.core.gml.WspmFixation;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.ui.internal.PdbUiUtils;
import org.kalypso.model.wspm.pdb.ui.internal.admin.PdbHandlerUtils;
import org.kalypso.model.wspm.pdb.ui.internal.content.ElementSelector;
import org.kalypso.model.wspm.pdb.ui.internal.content.IConnectionViewer;
import org.kalypso.model.wspm.pdb.ui.internal.content.PdbView;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.command.GmltreeHandlerUtils;
import org.kalypso.ui.editor.gmleditor.part.GmlTreeView;

/**
 * @author Gernot Belger
 */
public class CheckinPdbHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final Shell shell = HandlerUtil.getActiveShellChecked( event );
    final GmlTreeView gmlViewer = GmltreeHandlerUtils.getTreeViewerChecked( event );

    final IWorkbenchWindow window = HandlerUtil.getActiveWorkbenchWindowChecked( event );
    final String commandName = HandlerUtils.getCommandName( event );

    final IPdbConnection connection = PdbUiUtils.getConnectionChecked( window );

    final CommandableWorkspace workspace = gmlViewer.getWorkspace();
    final IStructuredSelection selection = gmlViewer.getSelection();

    /* Ask user to save project and do nothing on cancel */
    final IConnectionViewer viewer = PdbHandlerUtils.getConnectionViewerChecked( event );
    final PdbWspmProject project = viewer.getProject();
    if( !project.confirmProjectSave() )
      return null;

    final ICheckInWorker worker = checkinSelection( workspace, selection );
    if( !preInit( shell, commandName, connection, worker ) )
      return null;

    if( !checkPreconditions( shell, commandName, worker ) )
      return null;

    if( !showWizard( shell, commandName, worker ) )
      return null;

    final ElementSelector selector = new ElementSelector();
    worker.configureSelector( selector );
    PdbView.reloadViewAndBringtoTop( window, selector );

    saveLocalProject( shell, commandName, project );

    return null;
  }

  private boolean checkPreconditions( final Shell shell, final String windowTitle, final ICheckInWorker worker )
  {
    final IStatus status = worker.checkPreconditions();
    if( !status.isOK() )
    {
      StatusDialog.open( shell, status, windowTitle );
      return false;
    }

    return true;
  }

  private ICheckInWorker checkinSelection( final CommandableWorkspace workspace, final IStructuredSelection selection ) throws ExecutionException
  {
    final Object firstElement = selection.getFirstElement();
    if( firstElement instanceof TuhhReach )
      return new CheckinStateWorker( workspace, (TuhhReach)firstElement );

    if( firstElement instanceof WspmFixation )
      return new CheckInEventWorker( workspace, (WspmFixation)firstElement );

    throw new ExecutionException( "This handler only works on TuhhReach'es or WspmFixation's" ); //$NON-NLS-1$
  }

  private boolean preInit( final Shell shell, final String windowTitle, final IPdbConnection connection, final ICheckInWorker worker )
  {
    final ICoreRunnableWithProgress operation = new ICoreRunnableWithProgress()
    {
      @Override
      public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException, CoreException
      {
        try
        {
          monitor.beginTask( Messages.getString( "CheckinPdbHandler.0" ), IProgressMonitor.UNKNOWN ); //$NON-NLS-1$
          worker.preInit( connection );
          return Status.OK_STATUS;
        }
        catch( final PdbConnectException e )
        {
          throw new InvocationTargetException( e );
        }
      }
    };
    final IStatus result = ProgressUtilities.busyCursorWhile( operation );
    if( result.isOK() )
      return true;

    final String msg = Messages.getString( "CheckinPdbHandler.1" ); //$NON-NLS-1$
    ErrorDialog.openError( shell, windowTitle, msg, result );
    return false;
  }

  private boolean showWizard( final Shell shell, final String commandName, final ICheckInWorker worker )
  {
    final Wizard wizard = worker.createWizard();
    wizard.setWindowTitle( commandName );

    final WizardDialog2 dialog = new WizardDialog2( shell, wizard );
    dialog.setRememberSize( true );

    return dialog.open() == Window.OK;
  }

  private void saveLocalProject( final Shell shell, final String windowTitle, final PdbWspmProject project )
  {
    final ICoreRunnableWithProgress saveOperation = new ICoreRunnableWithProgress()
    {
      @Override
      public IStatus execute( final IProgressMonitor monitor ) throws CoreException
      {
        project.doSave( monitor );
        return Status.OK_STATUS;
      }
    };

    final IStatus result = ProgressUtilities.busyCursorWhile( saveOperation );
    if( !result.isOK() )
      StatusDialog.open( shell, result, windowTitle );
  }
}