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
package org.kalypso.portal.action;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.dialogs.DialogSettings;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.internal.dialogs.NewWizard;
import org.eclipse.ui.wizards.newresource.BasicNewProjectResourceWizard;
import org.kalypso.contribs.eclipse.core.resources.IProjectProvider;
import org.kalypso.portal.wizard.NewDssProjectWizard;
import org.kalypso.workflow.ui.browser.AbstractURLAction;
import org.kalypso.workflow.ui.browser.ICommandURL;

/**
 * example<br>
 * kalypso://selectProject?categoryId=org.kalypso.portal.loadProject.wizard
 * 
 * @author doemming
 */
public class URLActionSelectProject extends AbstractURLAction
{

  // e.g. "org.kalypso.portal.loadProject.wizard";
  private final static String PARAM_CATEGORY_ID = "categoryId";

  /**
   * @see org.kalypso.contribs.eclipse.ui.browser.commandable.ICommandURLAction#run(org.kalypso.contribs.eclipse.ui.browser.commandable.ICommandURL)
   */
  public boolean run( ICommandURL commandURL )
  {
    final String categoryID = commandURL.getParameter( PARAM_CATEGORY_ID );

    final NewWizard selectionWizard = new NewWizard();

    selectionWizard.init( getWorkbench(), new StructuredSelection() );
    selectionWizard.setCategoryId( categoryID );

    // TODO move class to other plugin
    final IDialogSettings dialogSettings = new DialogSettings( "empty" );
    // KalypsoPortalPlugin.getDefault().getDialogSettings();
    selectionWizard.setDialogSettings( dialogSettings );

    final WizardDialog dialog = new WizardDialog( getWorkbench().getActiveWorkbenchWindow().getShell(), selectionWizard );
    int open = dialog.open();
    if( open == Window.OK )
    {
      // get project handle
      final IWizardPage currentPage = dialog.getCurrentPage();
      if( currentPage != null )
      {
        final IWizard wizard = currentPage.getWizard();
        IProject selectedProject = null;
        if( wizard instanceof BasicNewProjectResourceWizard )
          selectedProject = ((BasicNewProjectResourceWizard) wizard).getNewProject();
        else if( wizard instanceof NewDssProjectWizard )
          selectedProject = ((NewDssProjectWizard) wizard).getNewProject();
        else if( wizard instanceof IProjectProvider )
          selectedProject = ((IProjectProvider) wizard).getProject();
        if( selectedProject != null )
        {
          getWorkFlowContext().setContextProject( selectedProject );
          return true;
        }
      }
    }
    return false;
  }
}
