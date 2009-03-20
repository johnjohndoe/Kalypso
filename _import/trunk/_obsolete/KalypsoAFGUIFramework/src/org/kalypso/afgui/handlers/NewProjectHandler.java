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
package org.kalypso.afgui.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.internal.dialogs.NewWizard;
import org.eclipse.ui.internal.ide.IDEWorkbenchPlugin;
import org.eclipse.ui.internal.ide.IIDEHelpContextIds;

/**
 * Just show the normal New-Project-Wizard.<br>
 * 
 * REMARK: Most of the code was copied from {@link org.eclipse.ui.actions.NewProjectAction} (could not be reused there).
 * 
 * @author Gernot Belger
 */
@SuppressWarnings("restriction")
public class NewProjectHandler extends AbstractHandler implements IHandler
{
  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  public Object execute( final ExecutionEvent event )
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();

    final ISelection selection = (ISelection) context.getVariable( ISources.ACTIVE_CURRENT_SELECTION_NAME );

    final IWorkbench workbench = PlatformUI.getWorkbench();
    final NewWizard wizard = new NewWizard();

    wizard.setCategoryId( "org.kalypso.ui.newwizards.kalypso" ); //$NON-NLS-1$
    wizard.setProjectsOnly( true );
    final IStructuredSelection selectionToPass;
    if( selection instanceof IStructuredSelection )
      selectionToPass = (IStructuredSelection) selection;
    else
      selectionToPass = StructuredSelection.EMPTY;

    wizard.init( workbench, selectionToPass );

    final IDialogSettings workbenchSettings = IDEWorkbenchPlugin.getDefault().getDialogSettings();
    IDialogSettings wizardSettings = workbenchSettings.getSection( "NewWizardAction" );//$NON-NLS-1$
    if( wizardSettings == null )
    {
      wizardSettings = workbenchSettings.addNewSection( "NewWizardAction" );//$NON-NLS-1$
    }
    wizard.setDialogSettings( wizardSettings );
    wizard.setForcePreviousAndNextButtons( true );

    // Create wizard dialog.
    final WizardDialog dialog = new WizardDialog( null, wizard );
    dialog.create();
    dialog.getShell().setSize( Math.max( 500, dialog.getShell().getSize().x ), 500 );
    PlatformUI.getWorkbench().getHelpSystem().setHelp( dialog.getShell(), IIDEHelpContextIds.NEW_PROJECT_WIZARD );

    // Open wizard.
    dialog.open();

    return null;
  }

}
