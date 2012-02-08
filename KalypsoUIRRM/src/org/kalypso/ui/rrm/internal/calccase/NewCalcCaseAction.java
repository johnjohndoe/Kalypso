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
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.handlers.HandlerUtil;
import org.kalypso.contribs.eclipse.core.commands.HandlerUtils;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.simulation.ui.wizards.createCalcCase.NewCalculationCaseWizard;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class NewCalcCaseAction extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IWorkbenchWindow window = HandlerUtil.getActiveWorkbenchWindowChecked( event );
    final Shell shell = HandlerUtil.getActiveShellChecked( event );

    // selektiertes verzeichnis finden
    final IWorkbenchPage activePage = window.getActivePage();

    // instead we directly access the navigator and its selection
    final IViewPart part = activePage.findView( IPageLayout.ID_RES_NAV );
    // selection may never be null, else we get a NullPointerExceltion in the Wizard-Pages
    final ISelection selection = part == null ? StructuredSelection.EMPTY : part.getViewSite().getSelectionProvider().getSelection();

    // NewCalcCaseWizard starten
    final IPath calculationGmlPath = RrmSimulation.getCalculationGmlPath();

    final NewCalculationCaseWizard wizard = new NewCalculationCaseWizard( calculationGmlPath.toString() );
    wizard.init( window.getWorkbench(), (IStructuredSelection) selection );

    final WizardDialog dlg = new WizardDialog( shell, wizard )
    {
      @Override
      public void updateButtons( )
      {
        super.updateButtons();

        final Button nextButton = getButton( IDialogConstants.NEXT_ID );
        getShell().setDefaultButton( nextButton );
      }
    };

    if( dlg.open() != Window.OK )
      return null;

    if( wizard.isUpdate() )
    {
      final IStatus updateStatus = doUpdateTimeseries( wizard.getNewFolder() );
      if( !updateStatus.isOK() )
        StatusDialog.open( shell, updateStatus, HandlerUtils.getCommandName( event ) );
    }

    return null;
  }

  private IStatus doUpdateTimeseries( final IFolder newFolder )
  {
    final WorkspaceModifyOperation operation = new UpdateCalcCaseOperation( new IFolder[] { newFolder } );
    return ProgressUtilities.busyCursorWhile( operation, Messages.getString("NewCalcCaseAction_0") ); //$NON-NLS-1$
  }
}
