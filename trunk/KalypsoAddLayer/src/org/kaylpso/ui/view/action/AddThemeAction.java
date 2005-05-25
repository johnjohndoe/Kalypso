package org.kaylpso.ui.view.action;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.ogc.gml.outline.GisMapOutlineViewer;
import org.kalypso.ogc.gml.outline.PluginMapOutlineAction;
import org.kalypso.ogc.gml.outline.PluginMapOutlineActionDelegate;
import org.kalypso.ui.internal.dialogs.KalypsoImportWizard;

//import org.kalypso.ui.action.wizard.ImportWmsSourceWizard;

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

public class AddThemeAction implements PluginMapOutlineAction
{

  /**
   * 
   * @see org.kalypso.ogc.gml.outline.PluginMapOutlineAction#run(org.kalypso.ogc.gml.outline.GisMapOutlineViewer)
   */
  public void run( GisMapOutlineViewer outlineviewer )
  {
    Shell shell = outlineviewer.getControl().getShell();

    KalypsoImportWizard wizard2 = new KalypsoImportWizard( outlineviewer );

    final WizardDialog dialog = new WizardDialog( shell, wizard2 );
    dialog.open();
  }

  /**
   * @see org.kalypso.ogc.gml.outline.PluginMapOutlineAction#selectionChanged(org.kalypso.ogc.gml.outline.PluginMapOutlineActionDelegate,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( PluginMapOutlineActionDelegate delegate, ISelection selection )
  {
    // nothing

    // some example stuff:
    
    //    if( selection instanceof IStructuredSelection && !selection.isEmpty() )
    //    {
    //      boolean enabled = ( ( (IStructuredSelection)selection ).getFirstElement()
    // instanceof GisTemplateFeatureTheme );
    //      delegate.setEnabled( enabled );
    //    }
    //    else
    //      delegate.setEnabled( false );

    //    delegate.setEnabled( !selection.isEmpty() );
  }

}