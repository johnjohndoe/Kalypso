/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.pdb.ui.internal.checkout;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;
import org.kalypso.contribs.eclipse.core.commands.HandlerUtils;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusDialog2;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.ui.internal.admin.PdbHandlerUtils;
import org.kalypso.model.wspm.pdb.ui.internal.content.IConnectionViewer;
import org.kalypso.model.wspm.pdb.ui.internal.wspm.PdbWspmProject;
import org.kalypso.model.wspm.pdb.wspm.CheckoutPdbData;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;

/**
 * @author Gernot Belger
 */
public class CheckoutPdbHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final Shell shell = HandlerUtil.getActiveShellChecked( event );
    final IStructuredSelection selection = (IStructuredSelection) HandlerUtil.getCurrentSelectionChecked( event );
    final IConnectionViewer viewer = PdbHandlerUtils.getConnectionViewerChecked( event );
    final String commandName = HandlerUtils.getCommandName( event );

    if( selection.isEmpty() )
    {
      final String message = "Please select at least one item in the tree.";
      MessageDialog.openInformation( shell, commandName, message );
      return null;
    }

    /* Ask user to save project and do nothing on cancel */
    final PdbWspmProject project = viewer.getProject();
    if( !project.confirmProjectSave() )
      return null;

    final CheckoutPdbData data = new CheckoutPdbData();
    initMapping( data, selection, project );

    final IPdbConnection connection = viewer.getConnection();

    final CheckoutPdbWizard wizard = new CheckoutPdbWizard( data );
    wizard.setWindowTitle( commandName );

    final IDialogSettings settings = wizard.getDialogSettings();
    data.init( shell, commandName, settings, connection );

    final WizardDialog dialog = new WizardDialog( shell, wizard );
    dialog.open();

    /* Always save project data now */
    doSaveProject( shell, project, commandName );

    final Object[] toSelect = data.getNewWspmElements();
    if( !ArrayUtils.isEmpty( toSelect ) )
      project.updateViews( toSelect );

    return null;
  }

  private void initMapping( final CheckoutPdbData data, final IStructuredSelection selection, final PdbWspmProject project )
  {
    final CommandableWorkspace workspace = project.getWorkspace();
    final TuhhWspmProject wspmProject = project.getWspmProject();
    data.initMapping( selection, workspace, wspmProject );
  }

  private void doSaveProject( final Shell shell, final PdbWspmProject project, final String windowTitle )
  {
    final IStatus saveStatus = ProgressUtilities.busyCursorWhile( new ICoreRunnableWithProgress()
    {
      @Override
      public IStatus execute( final IProgressMonitor monitor ) throws CoreException
      {
        project.doSave( monitor );
        return Status.OK_STATUS;
      }
    } );

    if( !saveStatus.isOK() )
      new StatusDialog2( shell, saveStatus, windowTitle ).open();
  }
}