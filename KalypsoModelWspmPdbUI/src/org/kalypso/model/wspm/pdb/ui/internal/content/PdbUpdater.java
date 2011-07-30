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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.hibernate.jdbc.Work;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusDialog2;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.db.PdbInfo;
import org.kalypso.model.wspm.pdb.db.version.UpdateScript;
import org.kalypso.model.wspm.pdb.db.version.UpdateScriptExtenions;
import org.kalypso.model.wspm.pdb.ui.internal.WorkRunnable;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.osgi.framework.Version;

/**
 * @author Gernot Belger
 */
public class PdbUpdater
{
  enum DbState
  {
    uptodate,
    empty,
    dbOutdated,
    kalypsoOutdated
  }

  private final static String WINDOW_TITLE = "Open Connection";

  private static final String STR_CONNECTION_IMPOSSIBLE = "Connection failed: ";

  private static final String STR_CREATE = STR_CONNECTION_IMPOSSIBLE + "database seems to be empty.";

  private static final String STR_UPDATE = STR_CONNECTION_IMPOSSIBLE + "database version (%s) is older than the current version (%s).";

  private final IPdbConnection m_connection;

  private final Shell m_shell;

  public PdbUpdater( final IPdbConnection connection, final Shell shell )
  {
    m_connection = connection;
    m_shell = shell;
  }

  public IStatus execute( )
  {
    /* Check version */
    final PdbInfo info = m_connection.getInfo();
    final boolean isSuperuser = checkSuperuser();
    final Version version = info.getVersion();
    final DbState state = checkDbState( version );

    switch( state )
    {
      case uptodate:
        return Status.OK_STATUS;

      case empty:
        if( isSuperuser )
          return handleCreate();
        else
          return handleShouldCreate();

      case kalypsoOutdated:
        final String msg = String.format( STR_CONNECTION_IMPOSSIBLE + "database version (%s) is newer than the version of Kalypso.\nPlease update Kalypso.", version );
        return new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, msg );

      case dbOutdated:
        if( isSuperuser )
          return handleUpdate( version );
        else
          return handleShouldUpdate( version );
    }

    throw new IllegalStateException();
  }

  private DbState checkDbState( final Version version )
  {
    // TODO: check, if database is empty
    // TODO: check, if it is a spatial database
    if( version == null )
      return DbState.empty;

    if( PdbInfo.CURRENT_VERSION.equals( version ) )
      return DbState.uptodate;

    if( version.compareTo( PdbInfo.CURRENT_VERSION ) > 0 )
      return DbState.kalypsoOutdated;

    if( version.compareTo( PdbInfo.CURRENT_VERSION ) < 0 )
      return DbState.dbOutdated;

    throw new IllegalStateException();
  }

  private boolean checkSuperuser( )
  {
    final String username = m_connection.getSettings().getUsername();
    return IPdbConnection.SUPERUSER.equals( username );
  }

  private IStatus handleCreate( )
  {
    /* ask user what to do */
    final String msg = String.format( "%s\nDo you wish to create the database?", STR_CREATE );
    final MessageDialog dialog = new MessageDialog( m_shell, WINDOW_TITLE, null, msg, MessageDialog.CONFIRM, new String[] { "Create Now!", IDialogConstants.CANCEL_LABEL }, 1 );
    if( dialog.open() != Window.OK )
      return Status.CANCEL_STATUS;

    /* Do update */
    final String dbType = m_connection.getSettings().getType();
    final UpdateScript createScript = UpdateScriptExtenions.getScript( new Version( 0, 0, 0 ), dbType );
    if( createScript == null )
      throw new IllegalStateException( "Missing create script" ); //$NON-NLS-1$
    final UpdateScript[] scripts = new UpdateScript[] { createScript };
    return executeScripts( scripts, "Create Database" );
  }

  private IStatus handleShouldCreate( )
  {
    final String msg = String.format( "%s\nLog-in with user '%s' to create the database.", STR_CREATE, IPdbConnection.SUPERUSER );
    MessageDialog.openWarning( m_shell, WINDOW_TITLE, msg );
    return Status.CANCEL_STATUS;
  }

  private IStatus handleUpdate( final Version version )
  {
    // 0) ggf. check preconditions

    /* ask user what to do */
    final String baseMsg = String.format( STR_UPDATE, version, PdbInfo.CURRENT_VERSION );
    final String msg = baseMsg + "\nPlease backup the database before updating.\nUpdate database now?";
    final MessageDialog dialog = new MessageDialog( m_shell, WINDOW_TITLE, null, msg, MessageDialog.CONFIRM, new String[] { "Update Now!", IDialogConstants.CANCEL_LABEL }, 1 );
    if( dialog.open() != Window.OK )
      return Status.CANCEL_STATUS;

    /* Do update */
    final String dbType = m_connection.getSettings().getType();
    final UpdateScript[] scripts = UpdateScriptExtenions.getUpdateScripts( version, dbType );
    if( scripts == null )
      throw new IllegalStateException( "Missing create script" ); //$NON-NLS-1$
    return executeScripts( scripts, "Update Database" );
  }

  private IStatus handleShouldUpdate( final Version version )
  {
    final String baseMsg = String.format( STR_UPDATE, version, PdbInfo.CURRENT_VERSION );
    final String msg = String.format( "%s\nLog-in with user '%s' to update.", baseMsg, IPdbConnection.SUPERUSER );
    MessageDialog.openWarning( m_shell, WINDOW_TITLE, msg );
    return Status.CANCEL_STATUS;
  }

  private IStatus executeScripts( final UpdateScript[] scripts, final String windowTitle )
  {
    final String[] sqls = loadSql( scripts );
    if( sqls == null )
      return Status.OK_STATUS;

    final Work operation = new SqlWork( sqls );
    final WorkRunnable runnable = new WorkRunnable( m_connection, operation );

    final IStatus status = ProgressUtilities.busyCursorWhile( runnable );
    if( !status.isOK() )
      new StatusDialog2( m_shell, status, windowTitle ).open();

    m_connection.updateInfo();

    return status;
  }

  private String[] loadSql( final UpdateScript[] scripts )
  {
    try
    {
      final Collection<String> sql = new ArrayList<String>();
      for( final UpdateScript script : scripts )
      {
        final String[] sqlStatements = script.loadSQL();
        sql.addAll( Arrays.asList( sqlStatements ) );
      }
      return sql.toArray( new String[sql.size()] );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new IllegalStateException( "Failed to load update scripts", e );
    }
  }
}