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
package org.kalypso.model.wspm.ui.action;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.model.wspm.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.wizard.CreateProfileDeviderWizard;
import org.kalypso.model.wspm.ui.wizard.FeatureThemeWizardUtilitites;
import org.kalypso.model.wspm.ui.wizard.FeatureThemeWizardUtilitites.FOUND_PROFILES;

/**
 * @author Gernot Belger
 */
public class CreateDeviderMapThemeAction implements IObjectActionDelegate
{
  private ISelection m_selection;

  private IWorkbenchPart m_targetPart;

  /**
   * @see org.eclipse.ui.IObjectActionDelegate#setActivePart(org.eclipse.jface.action.IAction,
   *      org.eclipse.ui.IWorkbenchPart)
   */
  public void setActivePart( final IAction action, final IWorkbenchPart targetPart )
  {
    m_targetPart = targetPart;

    action.setEnabled( m_targetPart != null );
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    /* retrieve selected profiles, abort if none */
    final FOUND_PROFILES foundProfiles = FeatureThemeWizardUtilitites.getProfileFeaturesFromThemeSelection( m_selection );

    final Shell shell = m_targetPart.getSite().getShell();

    if( foundProfiles == null || foundProfiles.foundProfiles.length == 0 )
    {
      MessageDialog.openWarning( shell, Messages.getString("org.kalypso.model.wspm.ui.action.CreateDeviderMapThemeAction.0"), Messages.getString("org.kalypso.model.wspm.ui.action.CreateDeviderMapThemeAction.1") ); //$NON-NLS-1$ //$NON-NLS-2$
      return;
    }

    final IWizard intersectWizard = new CreateProfileDeviderWizard( foundProfiles );

    /* show intersection wizard */
    final WizardDialog2 dialog = new WizardDialog2( shell, intersectWizard );
    dialog.setRememberSize( true );
    dialog.open();
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    m_selection = selection;
  }

}
