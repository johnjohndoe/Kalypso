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

import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.HandlerUtil;
import org.hibernate.Session;
import org.kalypso.commons.pair.IKeyValue;
import org.kalypso.commons.pair.KeyValueFactory;
import org.kalypso.model.wspm.pdb.PdbUtils;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.GetPdbList;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.preferences.PdbView;
import org.kalypso.ui.editor.gmleditor.command.GmltreeHandlerUtils;
import org.kalypso.ui.editor.gmleditor.part.GmlTreeView;

/**
 * @author Gernot Belger
 */
public class CheckinStateHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final Shell shell = HandlerUtil.getActiveShellChecked( event );
    final GmlTreeView gmlViewer = GmltreeHandlerUtils.getTreeViewerChecked( event );

    final IKeyValue<State[], WaterBody[]> existingData = getExistingData();

    final CheckinStateData data = new CheckinStateData( gmlViewer, existingData.getKey(), existingData.getValue() );

    final Wizard wizard = new CheckinStateWizard( data );
    final WizardDialog dialog = new WizardDialog( shell, wizard );
    dialog.open();

    return null;
  }

  private IKeyValue<State[], WaterBody[]> getExistingData( ) throws ExecutionException
  {
    final IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
    final PdbView view = new FindViewRunnable<PdbView>( PdbView.ID, window, true ).execute();
    if( view == null )
      throw new ExecutionException( "Failed to find Cross Section Database View" );

    final IPdbConnection connection = view.getConnection();
    if( connection == null )
      throw new ExecutionException( "Not connected to database" );

    Session session = null;
    try
    {
      session = connection.openSession();
      final List<State> states = GetPdbList.getList( session, State.class );
      final List<WaterBody> waterbodies = GetPdbList.getList( session, WaterBody.class );
      final State[] stateArray = states.toArray( new State[states.size()] );
      final WaterBody[] waterbodiesArray = waterbodies.toArray( new WaterBody[waterbodies.size()] );
      return KeyValueFactory.createPairEqualsBoth( stateArray, waterbodiesArray );
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
      throw new ExecutionException( "Failed to access database", e );
    }
    finally
    {
      PdbUtils.closeSessionQuietly( session );
    }
  }
}