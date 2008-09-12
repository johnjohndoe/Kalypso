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
package org.kalypso.workflow.ui.browser.urlaction;

import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.kalypso.ogc.gml.outline.GisMapOutlineViewer;
import org.kalypso.ui.KalypsoAddLayerPlugin;
import org.kalypso.ui.editor.mapeditor.GisMapEditor;
import org.kalypso.ui.editor.mapeditor.GisMapOutlinePage;
import org.kalypso.ui.view.action.KalypsoAddLayerWizard;
import org.kalypso.workflow.ui.browser.AbstractURLAction;
import org.kalypso.workflow.ui.browser.ICommandURL;

/**
 * It is assumed that the active editor is a GisMapEditor, if not use ready made commands to open or activate the
 * apporpriate Editor.
 * 
 * @see org.kalypso.workflow.ui.browser.urlaction.URLActionOpenEditor
 * @see org.kalypso.workflow.ui.browser.urlaction.URLActionActivateEditor
 * @author kuepfer
 */
public class URLActionAddThemeGMT extends AbstractURLAction
{

  /**
   * @see org.kalypso.workflow.ui.browser.IURLAction#run(org.kalypso.workflow.ui.browser.ICommandURL)
   */
  public boolean run( ICommandURL commandURL )
  {
    final IEditorPart activeEditor = getActiveEditor();
    if( activeEditor instanceof GisMapEditor )
    {
      final GisMapEditor gisMapEditor = (GisMapEditor) activeEditor;
      final IContentOutlinePage outlineView = (IContentOutlinePage) gisMapEditor.getAdapter( IContentOutlinePage.class );
      if( outlineView instanceof GisMapOutlinePage )
      {
        final GisMapOutlinePage gisOutlinePage = (GisMapOutlinePage) outlineView;
        final GisMapOutlineViewer viewer = gisOutlinePage.getModellView();
        final Shell shell = viewer.getControl().getShell();
        final KalypsoAddLayerWizard wizard = new KalypsoAddLayerWizard( gisOutlinePage );
        final IWorkbenchWindow activeWorkbenchWindow = KalypsoAddLayerPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow();
        wizard.init( activeWorkbenchWindow.getWorkbench() );
        wizard.setForcePreviousAndNextButtons( true );
        final WizardDialog dialog = new WizardDialog( shell, wizard );
        dialog.open();
        return true;
      }
    }
    return false;
  }

}
