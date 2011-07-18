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
package org.kalypso.model.wspm.pdb.ui.internal.content;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusDialog2;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.IPdbOperation;
import org.kalypso.model.wspm.pdb.db.PdbInfo;
import org.kalypso.model.wspm.pdb.ui.internal.ExecutorRunnable;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.osgi.framework.Version;

/**
 * @author Gernot Belger
 *
 */
public class PdbUpdater
{
  private final IPdbConnection m_connection;

  private final PdbInfo m_info;

  private final Shell m_shell;

  public PdbUpdater( final IPdbConnection connection, final PdbInfo info, final Shell shell )
  {
    m_connection = connection;
    m_info = info;
    m_shell = shell;
  }

  public IStatus execute( )
  {
    /* Check version */
    final Version version = m_info.getVersion();
    if( version == null )
      return handleCreate();

    if( PdbInfo.CURRENT_VERSION.equals( version ) )
      return Status.OK_STATUS;

    if( version.compareTo( PdbInfo.CURRENT_VERSION ) > 0 )
    {
      final String msg = String.format( "The Version of the cross section database (%s) is newer than your version of Kalypso. Connection impossible. Please update your Kalypso.", version );
      return new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, msg );
    }

    if( version.compareTo( PdbInfo.CURRENT_VERSION ) < 0 )
      return handleUpdate( version );

    throw new IllegalStateException();
  }

  private IStatus handleCreate( )
  {
    // TODO: check, if database is empty
    // TODO: check, if it is a spatial database

    /* ask user what to do */
    final String msg = String.format( "You are connection with an empty database.\nDo you wish to create the database with the current user as owner?\n" );
    final MessageDialog dialog = new MessageDialog( m_shell, "Create Database", null, msg, MessageDialog.CONFIRM, new String[] { "Create Now!", IDialogConstants.CANCEL_LABEL }, 1 );
    if( dialog.open() != Window.OK )
      return Status.CANCEL_STATUS;


    /* Do update */
    final String dbType = m_connection.getSettings().getType();
    final IPdbOperation operation = new PdbCreateOperation( dbType );
    final ExecutorRunnable runnable = new ExecutorRunnable( m_connection, operation );

    final IStatus status = ProgressUtilities.busyCursorWhile( runnable );
    if( !status.isOK() )
      new StatusDialog2( m_shell, status, "Update Failed" ).open();

    return status;
  }

  private IStatus handleUpdate( final Version version )
  {
    // 0) ggf. check preconditions

    /* ask user what to do */
    final String msg = String.format( "The Version of the cross section database (%s) is older than the current version (%s).\nPlease backup the database before updating.\nUpdate database now?", version, PdbInfo.CURRENT_VERSION );
    final MessageDialog dialog = new MessageDialog( m_shell, "Update Database", null, msg, MessageDialog.CONFIRM, new String[] { "Update Now!", IDialogConstants.CANCEL_LABEL }, 1 );
    if( dialog.open() != Window.OK )
      return Status.CANCEL_STATUS;

    /* Do update */
    final String dbType = m_connection.getSettings().getType();
    final IPdbOperation operation = new PdbUpdateOperation( version, dbType );
    final ExecutorRunnable runnable = new ExecutorRunnable( m_connection, operation );

    // FIXME: we need to update the info now

    final IStatus status = ProgressUtilities.busyCursorWhile( runnable );
    if( !status.isOK() )
      new StatusDialog2( m_shell, status, "Update Failed" ).open();

    return status;
  }
}