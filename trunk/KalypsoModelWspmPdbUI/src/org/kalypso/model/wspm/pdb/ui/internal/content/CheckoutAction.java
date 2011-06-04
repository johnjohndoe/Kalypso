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

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusDialog2;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiImages;
import org.kalypso.model.wspm.pdb.ui.internal.wspm.PdbWspmProject;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;

/**
 * @author Gernot Belger
 */
public class CheckoutAction extends Action
{
  private final ConnectionContentControl m_control;

  public CheckoutAction( final ConnectionContentControl control )
  {
    m_control = control;

    setText( "Download selected items" );
    setToolTipText( "Download the selected items into the local workspace." );
    setImageDescriptor( WspmPdbUiImages.getImageDescriptor( WspmPdbUiImages.IMAGE.IMPORT ) );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();

    final IStructuredSelection selection = m_control.getSelection();
    if( selection.isEmpty() )
    {
      final String message = "Please select at least one item in the tree.";
      MessageDialog.openInformation( shell, getText(), message );
      return;
    }

    // TODO: ask, if all local data should be replaced (or at least an existing copy of the reach should be replaced)

    final PdbWspmProject project = m_control.getProject();
    /* Ask user to save project and do nothing on cancel */
    if( !project.saveProject( true ) )
      return;

    final CheckoutOperation operation = new CheckoutOperation( project, selection );
    final IStatus status = ProgressUtilities.busyCursorWhile( operation );
    if( !status.isOK() )
      new StatusDialog2( shell, status, getText() ).open();

    final TuhhReach[] toSelect = operation.getNewReaches();
    project.updateViews( toSelect );
  }
}