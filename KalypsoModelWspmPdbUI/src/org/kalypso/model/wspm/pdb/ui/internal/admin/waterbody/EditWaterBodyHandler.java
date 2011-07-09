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

import org.apache.commons.lang.ObjectUtils;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;
import org.hibernate.Session;
import org.kalypso.core.status.StatusDialog2;
import org.kalypso.model.wspm.pdb.PdbUtils;
import org.kalypso.model.wspm.pdb.connect.Executor;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.FlushOperation;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.admin.PdbHandlerUtils;
import org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.EditWaterBodyPage.Mode;
import org.kalypso.model.wspm.pdb.ui.internal.content.ElementSelector;
import org.kalypso.model.wspm.pdb.ui.internal.content.IConnectionViewer;

/**
 * @author Gernot Belger
 */
public class EditWaterBodyHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final Shell shell = HandlerUtil.getActiveShellChecked( event );
    final WaterBody selectedItem = PdbHandlerUtils.getSelectedWaterBodyChecked( event );

    final IConnectionViewer viewer = PdbHandlerUtils.getConnectionViewerChecked( event );

    final String windowTitle = "Edit Water Body";

    String nameToSelect = selectedItem.getName();
    Session session = null;
    try
    {
      session = PdbHandlerUtils.aquireSession( viewer );

      // TODO Show busy cursor...
      final WaterBody[] existingWaterbodies = WaterBodyViewer.loadWaterbodies( session );
      final WaterBody waterBodyToEdit = findWaterBody( existingWaterbodies, selectedItem );

      final WaterBody clone = cloneForEdit( waterBodyToEdit );

      final EditWaterBodyWizard wizard = new EditWaterBodyWizard( existingWaterbodies, clone, Mode.EDIT );
      wizard.setWindowTitle( windowTitle );

      final WizardDialog dialog = new WizardDialog( shell, wizard );
      if( dialog.open() == Window.OK )
      {
        nameToSelect = clone.getName();
        uncloneData( waterBodyToEdit, clone );

        final FlushOperation operation = new FlushOperation();
        new Executor( session, operation ).execute();
      }

      session.close();
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, e.getLocalizedMessage(), e );
      new StatusDialog2( shell, status, windowTitle ).open();
    }
    finally
    {
      PdbUtils.closeSessionQuietly( session );
    }

    final ElementSelector selector = new ElementSelector();
    selector.addWaterBodyName( nameToSelect );
    viewer.reload( selector );
    return null;
  }

  private WaterBody findWaterBody( final WaterBody[] waterbodies, final WaterBody element )
  {
    final String name = element.getName();
    for( final WaterBody waterBody : waterbodies )
    {
      if( ObjectUtils.equals( waterBody.getName(), name ) )
        return waterBody;

    }
    return null;
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
}