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

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.handlers.HandlerUtil;
import org.kalypso.contribs.eclipse.core.commands.HandlerUtils;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.ui.internal.PdbUiUtils;
import org.kalypso.model.wspm.pdb.ui.internal.preferences.PdbView;
import org.kalypso.model.wspm.pdb.wspm.CheckinStateData;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
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

    final IWorkbenchWindow window = HandlerUtil.getActiveWorkbenchWindowChecked( event );

    final IPdbConnection connection = PdbUiUtils.getConnectionChecked( window );

    final CommandableWorkspace workspace = gmlViewer.getWorkspace();
    final IStructuredSelection selection = gmlViewer.getSelection();

    final CheckinStateData data = new CheckinStateData( workspace, selection );
    try
    {
      data.init( connection );
    }
    catch( final PdbConnectException e )
    {
      e.printStackTrace();
      throw new ExecutionException( "Failed to access database", e );
    }

    final String commandName = HandlerUtils.getCommandName( event );
    final CheckinStateWizard wizard = new CheckinStateWizard( data, connection );
    wizard.setWindowTitle( commandName );
    final WizardDialog dialog = new WizardDialog( shell, wizard );
    dialog.open();

    final IStatus status = wizard.getStatus();
    if( status != null && status.isOK() )
    {
      final String newStateName = data.getState().getName();
      PdbView.reloadViewAndBringtoTop( window, newStateName );
    }

    return null;
  }
}