/*
 * --------------- Kalypso-Header
 * --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal
 * engineering Denickestr. 22 21073 Hamburg, Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany
 * http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.floodrisk.action;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.kalypso.eclipse.core.resources.ProjectUtilities;
import org.kalypso.floodrisk.process.ProcessExtension;
import org.kalypso.floodrisk.process.ProcessExtensionReader;
import org.kalypso.floodrisk.wizard.ChooseProcessDialog;
import org.kalypso.floodrisk.wizard.ProcessInputWizard;

/**
 * StartCalculationActionDelegate
 * <p>
 * 
 * created by
 * 
 * @author Nadja Peiler (13.05.2005)
 */
public class StartCalculationActionDelegate implements
    IWorkbenchWindowActionDelegate
{
  private IWorkbenchWindow m_window;

  /**
   * 
   * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#dispose()
   */
  public void dispose()
  {
    //nothing
  }

  /**
   * 
   * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#init(org.eclipse.ui.IWorkbenchWindow)
   */
  public void init( IWorkbenchWindow window )
  {
    m_window = window;
  }

  /**
   * open ProcessDialog and InputWizard to start one or more processes
   * 
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( IAction action )
  {
    //System.out.println( "Run..." );
    try
    {
      if( ProjectUtilities.getSelectedProjects().length > 0 )
      {
        final IProject firstSelectedProject = ProjectUtilities
            .getSelectedProjects()[0];
        final ProcessExtension[] processes = ProcessExtensionReader
            .retrieveExtensions();
        ChooseProcessDialog dialog = new ChooseProcessDialog( m_window
            .getShell(), processes );
        int open = dialog.open();
        if( open == Window.OK )
        {
          ProcessInputWizard processInputWizard = new ProcessInputWizard(
              firstSelectedProject, dialog.getProcesses() );
          final WizardDialog wizardDialog = new WizardDialog( m_window
              .getShell(), processInputWizard );
          wizardDialog.open();
        }
        else
        {
          //nothing
        }
      }
      else
      {
        MessageDialog.openError( m_window.getShell(), "Error",
            "Bitte Projekt auswählen!" );
      }
    }
    catch( CoreException e )
    {
      e.printStackTrace();
    }
  }

  /**
   * 
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
    //nothing
  }

}