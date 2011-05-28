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
package org.kalypso.model.wspm.pdb.ui.internal.admin.gaf;

import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.hibernate.Session;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.core.status.StatusDialog2;
import org.kalypso.model.wspm.pdb.connect.ConnectionUtils;
import org.kalypso.model.wspm.pdb.connect.Executor;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.ListOperation;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiImages;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;

/**
 * @author Gernot Belger
 */
public class ImportGafAction extends Action
{
  private final IPdbConnection m_connection;

  private final IUpdateable m_updateable;

  public ImportGafAction( final IPdbConnection connection, final IUpdateable updateable )
  {
    m_connection = connection;
    m_updateable = updateable;

    setText( "GAF Daten importieren..." );
    setImageDescriptor( WspmPdbUiImages.getImageDescriptor( WspmPdbUiImages.IMAGE.IMPORT ) );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();

    Session session = null;
    try
    {
      session = m_connection.openSession();
      final State[] states = getState( session );

      final String username = m_connection.getSettings().getUsername();
      final Wizard importWizard = new ImportGafWizard( session, states, username );
      importWizard.setWindowTitle( "Import GAF file" );
      final WizardDialog dialog = new WizardDialog( shell, importWizard );
      final int result = dialog.open();
      session.close();
      if( result == Window.OK )
        m_updateable.update();
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, "Error during GAF Import", e );
      new StatusDialog2( shell, status, "GAF Import" ).open();
    }
    finally
    {
      ConnectionUtils.closeSessionQuietly( session );
    }
  }

  private State[] getState( final Session session ) throws PdbConnectException
  {
    final ListOperation<State> operation = new ListOperation<State>( State.class );
    new Executor( session, operation ).execute();
    final List<State> list = operation.getList();
    return list.toArray( new State[list.size()] );
  }
}