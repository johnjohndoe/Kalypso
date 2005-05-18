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
package org.kalypso.floodrisk.wizard;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.IWorkbench;
import org.kalypso.floodrisk.process.ProcessExtension;

/**
 * ProcessWizard
 * <p>
 * 
 * created by
 * 
 * @author Nadja Peiler (13.05.2005)
 */
public class ChooseProcessWizard extends Wizard
{
  private IProject m_project;

  private ProcessExtension[] m_processes;

  private ChooseProcessWizardPage m_page1;

  private Object m_workbench;

  private Object currentSelection;

  private Object m_selection;

  public ChooseProcessWizard( IProject project, ProcessExtension[] processes )
  {
    super();
    m_project = project;
    m_processes = processes;
    setWindowTitle( "Choose Process Wizard" );
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#addPages()
   */
  public void addPages()
  {
    super.addPages();

    m_page1 = new ChooseProcessWizardPage( "ChooseCalculationsWizardPage", "Choose calculations",
        null );
    addPage( m_page1 );
    m_page1.setProcesses( m_processes );

  }

  public boolean performFinish()
  {
    ProcessExtension[] processes = m_page1.getProcesses();
    //for( int i = 0; i < processes.length; i++ )
    //{
    //  ProcessExtension pe = processes[i];
    //  System.out.println( "Process " + i + ": " + pe.getName() + " = " + pe.getState() );
    //}
    ProcessInputWizard processInputWizard = new ProcessInputWizard( m_project, processes );
    final WizardDialog dialog = new WizardDialog( getShell(), processInputWizard);
    dialog.open();
    return true;
  }

  public void init( IWorkbench workbench, IStructuredSelection selection )
  {
    m_workbench = workbench;
    m_selection = selection;
  }

}