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
package org.kalypso.ui.view.action;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindow;
import org.kalypso.ogc.gml.outline.GisMapOutlineViewer;
import org.kalypso.ogc.gml.outline.PluginMapOutlineAction;
import org.kalypso.ogc.gml.outline.PluginMapOutlineActionDelegate;
import org.kalypso.ui.KalypsoAddLayerPlugin;

public class AddThemeAction implements PluginMapOutlineAction
{
  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( IAction action )
  {
    if( action instanceof PluginMapOutlineActionDelegate )
    {
      final GisMapOutlineViewer viewer = ((PluginMapOutlineActionDelegate) action).getOutlineviewer();
      final Shell shell = viewer.getControl().getShell();
      final KalypsoAddLayerWizard wizard = new KalypsoAddLayerWizard( viewer );
      final IWorkbenchWindow activeWorkbenchWindow = KalypsoAddLayerPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow();
      wizard.init( activeWorkbenchWindow.getWorkbench() );
      wizard.setForcePreviousAndNextButtons( true );
      final WizardDialog dialog = new WizardDialog( shell, wizard );
      dialog.open();
    }
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
    if( action instanceof PluginMapOutlineActionDelegate )
    {
      final GisMapOutlineViewer viewer = ((PluginMapOutlineActionDelegate) action).getOutlineviewer();
      if( viewer == null )
        action.setEnabled( false );
      else
        action.setEnabled( true );
    }
  }

}