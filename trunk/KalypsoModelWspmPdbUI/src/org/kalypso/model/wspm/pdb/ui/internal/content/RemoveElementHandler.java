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

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.IPdbOperation;
import org.kalypso.model.wspm.pdb.connect.command.ExecutorRunnable;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.admin.PdbHandlerUtils;
import org.kalypso.model.wspm.pdb.ui.internal.admin.event.RemoveEventWorker;
import org.kalypso.model.wspm.pdb.ui.internal.admin.state.RemoveStateWorker;
import org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.RemoveWaterBodyWorker;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class RemoveElementHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final Shell shell = HandlerUtil.getActiveShellChecked( event );
    final Object selectedItem = PdbHandlerUtils.getSelectedElementChecked( event );

    final IConnectionViewer viewer = PdbHandlerUtils.getConnectionViewerChecked( event );

    final IRemoveWorker worker = findWorker( selectedItem );

    if( !worker.checkPrerequisites( shell ) )
      return null;

    final IPdbConnection connection = viewer.getConnection();

    final IPdbOperation operation = worker.createOperation();
    final ExecutorRunnable runnable = new ExecutorRunnable( connection, operation );
    runnable.setOKStatus( new Status( IStatus.OK, WspmPdbUiPlugin.PLUGIN_ID, Messages.getString( "RemoveElementHandler.0" ) ) ); //$NON-NLS-1$

    final IStatus result = ProgressUtilities.busyCursorWhile( runnable );
    new StatusDialog( shell, result, worker.getWindowTitle() ).open();

    final ElementSelector selector = new ElementSelector();
    worker.addElementsToSelect( viewer, selector );
    viewer.reload( selector );
    return null;
  }

  private IRemoveWorker findWorker( final Object selectedItem )
  {
    if( selectedItem instanceof WaterBody )
      return new RemoveWaterBodyWorker( (WaterBody) selectedItem );

    if( selectedItem instanceof State )
      return new RemoveStateWorker( (State) selectedItem );

    if( selectedItem instanceof Event )
      return new RemoveEventWorker( (Event) selectedItem );

    throw new IllegalArgumentException();
  }
}