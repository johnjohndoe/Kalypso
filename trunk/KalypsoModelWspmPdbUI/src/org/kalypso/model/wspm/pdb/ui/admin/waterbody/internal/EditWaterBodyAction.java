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
package org.kalypso.model.wspm.pdb.ui.admin.waterbody.internal;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.hibernate.Session;
import org.kalypso.core.status.StatusDialog2;
import org.kalypso.model.wspm.pdb.connect.Executor;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.UpdateObjectCommand;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBodies;
import org.kalypso.model.wspm.pdb.ui.admin.waterbody.internal.EditWaterBodyPage.Mode;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;

/**
 * @author Gernot Belger
 */
public class EditWaterBodyAction extends WaterBodyAction
{
  private final ManageWaterBodiesPage m_page;

  private final WaterBodyViewer m_viewer;

  public EditWaterBodyAction( final ManageWaterBodiesPage page, final WaterBodyViewer viewer )
  {
    m_viewer = viewer;
    m_page = page;

    setText( "&Edit..." );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();

    final Session session = m_page.getSession();
    final WaterBodies[] existingWaterbodies = m_viewer.getExistingWaterbodies();

    final WaterBodies selectedItem = m_page.getSelectedItem();

    final String oldID = selectedItem.getWaterBody();

    final EditWaterBodyWizard wizard = new EditWaterBodyWizard( existingWaterbodies, selectedItem, Mode.EDIT );
    wizard.setWindowTitle( "Edit Water Body" );

    final WizardDialog dialog = new WizardDialog( shell, wizard );
    try
    {
      if( dialog.open() == Window.OK )
      {
        final String newID = selectedItem.getWaterBody();
        Assert.isTrue( newID.equals( oldID ) );

        final UpdateObjectCommand operation = new UpdateObjectCommand( selectedItem );
        new Executor( session, operation ).execute();
      }
      else
      {
        session.refresh( selectedItem );
      }
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, e.getLocalizedMessage(), e );
      new StatusDialog2( shell, status, wizard.getWindowTitle() ).open();
    }

    m_viewer.refreshWaterBodies( oldID );
  }

  @Override
  protected boolean checkEnabled( )
  {
    return m_page.getSelectedItem() != null;
  }
}