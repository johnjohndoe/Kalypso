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
package org.kalypso.ui.action;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.kalypso.eclipse.core.resources.ProjectUtilities;
import org.kalypso.ui.nature.prognose.OrganisePrognosesDialog;

/**
 *
 * Actions, um den Editor zum Organisieren der Vorhersagevarianten zu �ffnen.
 *
 * @author belger
 */
public class ManagePrognosesAction implements IWorkbenchWindowActionDelegate
{
  private IWorkbenchWindow m_window;

  /**
   * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#dispose()
   */
  public void dispose()
  {
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#init(org.eclipse.ui.IWorkbenchWindow)
   */
  public void init( final IWorkbenchWindow window )
  {
    m_window = window;
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    final ISelection selection = m_window.getSelectionService().getSelection( IPageLayout.ID_RES_NAV );

    final IProject[] projects = ProjectUtilities.findProjectsFromSelection( selection );

    if( projects == null || projects.length != 1 )
    {
      MessageDialog.openInformation( m_window.getShell(), "Vorhersagevarianten organisieren",
          "Bitte w�hlen Sie genau ein Projekt im Navigator aus" );
      return;
    }

    final TitleAreaDialog dialog = new OrganisePrognosesDialog( m_window.getShell(), projects[0] );
    dialog.open();
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    // egal, ich nehme eh die nur selection vom navigator 
  }
}
