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
package org.kalypso.model.wspm.pdb.ui.admin.waterbody.internal;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.core.status.StatusDialog2;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.DeleteObjectCommand;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBodies;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;

/**
 * @author Gernot Belger
 *
 */
public class RemoveWaterBodyAction extends WaterBodyAction
{
  private final ManageWaterBodiesPage m_page;

  private final WaterBodyViewer m_viewer;

  public RemoveWaterBodyAction( final ManageWaterBodiesPage page, final WaterBodyViewer viewer )
  {
    m_page = page;
    m_viewer = viewer;

    setText( "&Remove..." );
  }

  @Override
  protected boolean checkEnabled( )
  {
    return m_page.getSelectedItem() != null;
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();

    final IPdbConnection connection = m_page.getConnection();

    final WaterBodies selectedItem = m_page.getSelectedItem();
    final String id = selectedItem.getWaterBody();
    final String dialogTitle = "Remove Water Body";

    try
    {
      final boolean hasData = hasData( selectedItem );

      if( hasData )
      {
        /* show dialog with states/cross-sections -> water cannot be removed */
        final String message = "There are cross sections referencing this water body. Water body cannot be removed.";
        MessageDialog.openInformation( shell, dialogTitle, message );
        return;
      }
      else
      {
        final String message = String.format( "Remove waterbody: %s (%s)? This operation cannot be undone.", selectedItem.getName(), id );
        if( !MessageDialog.openConfirm( shell, dialogTitle, message ) )
          return;

        connection.executeCommand( new DeleteObjectCommand( selectedItem ) );
      }
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, e.getLocalizedMessage(), e );
      new StatusDialog2( shell, status, dialogTitle ).open();
    }

    m_viewer.refreshWaterBodies( id );
  }

  private boolean hasData( final WaterBodies waterBody )
  {
    return !waterBody.getCrossSectionses().isEmpty();
  }
}