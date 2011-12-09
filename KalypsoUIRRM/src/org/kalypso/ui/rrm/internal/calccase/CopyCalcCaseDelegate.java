/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.rrm.internal.calccase;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.actions.CopyFilesAndFoldersOperation;
import org.eclipse.ui.handlers.HandlerUtil;
import org.kalypso.model.hydrology.project.INaCalcCaseConstants;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class CopyCalcCaseDelegate extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IWorkbenchWindow window = HandlerUtil.getActiveWorkbenchWindowChecked( event );
    final Shell shell = HandlerUtil.getActiveShellChecked( event );

    final ISelection selection = window.getSelectionService().getSelection( IPageLayout.ID_RES_NAV );

    IResource resource = null;
    if( selection instanceof IStructuredSelection )
    {
      final IStructuredSelection struct = (IStructuredSelection) selection;
      if( struct.size() == 1 )
        resource = (IResource) struct.getFirstElement();
    }

    if( resource == null || !(resource instanceof IFolder) )
    {
      MessageDialog.openInformation( shell, Messages.getString( "org.kalypso.simulation.ui.actions.CopyCalcCaseDelegate.0" ), Messages.getString( "org.kalypso.simulation.ui.actions.CopyCalcCaseDelegate.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      return null;
    }

    final IFolder folder = (IFolder) resource;
    final IFile file = folder.getFile( INaCalcCaseConstants.CALCULATION_GML_PATH );
    if( !file.exists() )
    {
      MessageDialog.openInformation( shell, Messages.getString( "org.kalypso.simulation.ui.actions.CopyCalcCaseDelegate.0" ), Messages.getString( "org.kalypso.simulation.ui.actions.CopyCalcCaseDelegate.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      return null;
    }

    final CopyFilesAndFoldersOperation operation = new CopyFilesAndFoldersOperation( shell );
    operation.copyResources( new IResource[] { resource }, resource.getParent() );

    return null;
  }
}
