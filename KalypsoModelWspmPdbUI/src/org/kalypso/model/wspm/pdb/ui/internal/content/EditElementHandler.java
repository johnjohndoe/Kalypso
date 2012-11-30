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
package org.kalypso.model.wspm.pdb.ui.internal.content;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;
import org.hibernate.Session;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.wspm.pdb.PdbUtils;
import org.kalypso.model.wspm.pdb.connect.Executor;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.FlushOperation;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.admin.PdbHandlerUtils;
import org.kalypso.model.wspm.pdb.ui.internal.admin.event.EditEventWorker;
import org.kalypso.model.wspm.pdb.ui.internal.admin.state.EditStateWorker;
import org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.EditWaterBodyWorker;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class EditElementHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final Shell shell = HandlerUtil.getActiveShellChecked( event );
    final Object selectedItem = PdbHandlerUtils.getSelectedElementChecked( event );

    final IConnectionViewer viewer = PdbHandlerUtils.getConnectionViewerChecked( event );

    final String username = viewer.getUsername();

    final IPdbConnection connection = viewer.getConnection();

    final IEditWorker worker = findWorker( connection, selectedItem, username );

    Session session = null;
    try
    {
      session = PdbHandlerUtils.aquireSession( viewer );

      final Wizard wizard = createWizard( worker, session, shell );
      if( wizard == null )
        return null;
      wizard.setWindowTitle( worker.getWindowTitle() );

      final WizardDialog dialog = new WizardDialog( shell, wizard );
      if( dialog.open() == Window.OK )
      {
        worker.afterWizardOK();

        final FlushOperation operation = new FlushOperation();
        new Executor( session, operation ).execute();
      }

      session.close();
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, e.getLocalizedMessage(), e );
      final String windowTitle = worker.getWindowTitle();
      new StatusDialog( shell, status, windowTitle ).open();
    }
    finally
    {
      PdbUtils.closeSessionQuietly( session );
    }

    final ElementSelector selector = new ElementSelector();
    worker.addElementsToSelect( selector );
    viewer.reload( selector );
    return null;
  }

  private Wizard createWizard( final IEditWorker worker, final Session session, final Shell shell )
  {
    final Wizard[] wizards = new Wizard[1];
    final ICoreRunnableWithProgress operation = new ICoreRunnableWithProgress()
    {
      @Override
      public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
      {
        try
        {
          monitor.beginTask( Messages.getString( "EditElementHandler.0" ), IProgressMonitor.UNKNOWN ); //$NON-NLS-1$
          wizards[0] = worker.createWizard( monitor, session );
          return Status.OK_STATUS;
        }
        catch( final PdbConnectException e )
        {
          throw new InvocationTargetException( e );
        }
      }
    };

    final IStatus status = ProgressUtilities.busyCursorWhile( operation );
    if( !status.isOK() )
      new StatusDialog( shell, status, worker.getWindowTitle() ).open();

    return wizards[0];
  }

  private IEditWorker findWorker( final IPdbConnection connection, final Object selectedItem, final String username )
  {
    if( selectedItem instanceof WaterBody )
      return new EditWaterBodyWorker( (WaterBody)selectedItem );

    if( selectedItem instanceof State )
      return new EditStateWorker( (State)selectedItem, username );

    if( selectedItem instanceof Event )
      return new EditEventWorker( connection, (Event)selectedItem, username );

    throw new IllegalArgumentException();
  }
}