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
package org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.hibernate.Session;
import org.kalypso.contribs.eclipse.jface.action.UpdateableAction;
import org.kalypso.core.status.StatusDialog2;
import org.kalypso.model.wspm.pdb.connect.Executor;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.FlushOperation;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.EditWaterBodyPage.Mode;

/**
 * @author Gernot Belger
 */
public class EditWaterBodyAction extends UpdateableAction
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
    final WaterBody[] existingWaterbodies = m_viewer.getExistingWaterbodies();

    final WaterBody selectedItem = m_page.getSelectedItem();

    final WaterBody clone = cloneForEdit( selectedItem );

    final EditWaterBodyWizard wizard = new EditWaterBodyWizard( existingWaterbodies, clone, Mode.EDIT );
    wizard.setWindowTitle( "Edit Water Body" );

    final WizardDialog dialog = new WizardDialog( shell, wizard );
    try
    {
      if( dialog.open() == Window.OK )
      {
        uncloneData( selectedItem, clone );

        final FlushOperation operation = new FlushOperation();
        new Executor( session, operation ).execute();
      }
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, e.getLocalizedMessage(), e );
      new StatusDialog2( shell, status, wizard.getWindowTitle() ).open();
    }

    m_viewer.refreshWaterBody( selectedItem.getName() );
  }

  private WaterBody cloneForEdit( final WaterBody other )
  {
    final WaterBody clone = new WaterBody( other.getId(), other.getName(), other.getLabel(), other.getDirectionOfStationing() );
    clone.setDescription( other.getDescription() );
    clone.setRiverline( other.getRiverline() );
    return clone;
  }

  /**
   * Copy the edited data back into the persistent object.
   */
  private void uncloneData( final WaterBody original, final WaterBody clone )
  {
    original.setName( clone.getName() );
    original.setLabel( clone.getLabel() );
    original.setDescription( clone.getDescription() );
    original.setDirectionOfStationing( clone.getDirectionOfStationing() );
    original.setRiverline( clone.getRiverline() );
  }

  @Override
  protected boolean checkEnabled( )
  {
    return m_page.getSelectedItem() != null;
  }
}