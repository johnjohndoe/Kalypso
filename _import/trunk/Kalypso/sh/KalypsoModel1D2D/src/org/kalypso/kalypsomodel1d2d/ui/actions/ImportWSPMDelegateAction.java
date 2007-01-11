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
package org.kalypso.kalypsomodel1d2d.ui.actions;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartSite;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.kalypsomodel1d2d.schema.binding.FE1D2DDiscretisationModel;
import org.kalypso.kalypsomodel1d2d.ui.wizard.ImportWspmWizard;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class ImportWSPMDelegateAction implements IObjectActionDelegate
{
  private IAction m_action;

  private IWorkbenchPart m_targetPart;

  private IStructuredSelection m_selection;

  /**
   * @see org.eclipse.ui.IObjectActionDelegate#setActivePart(org.eclipse.jface.action.IAction,
   *      org.eclipse.ui.IWorkbenchPart)
   */
  public void setActivePart( final IAction action, final IWorkbenchPart targetPart )
  {
    m_action = action;
    m_targetPart = targetPart;
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    final IWorkbenchPartSite site = m_targetPart.getSite();
    final Shell shell = site.getShell();
    final IWorkbench workbench = site.getWorkbenchWindow().getWorkbench();

    final ImportWspmWizard importWizard = new ImportWspmWizard( );
    importWizard.init( workbench, m_selection );
    
    final WizardDialog2 dialog = new WizardDialog2( shell, importWizard );
    if( dialog.open() == WizardDialog2.OK )
    {
//      final IFile wspmModel = dialog.getModel2Import();
//      final String[ ] results = dialog.getResults2Import();
//      
//      WspmImporter.import( model, wspmModel, results );
    }
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    m_action = action;
    m_selection = (IStructuredSelection) selection;
  }

}
