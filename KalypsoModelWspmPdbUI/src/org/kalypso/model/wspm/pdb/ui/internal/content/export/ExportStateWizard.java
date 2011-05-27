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
package org.kalypso.model.wspm.pdb.ui.internal.content.export;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.internal.IWorkbenchGraphicConstants;
import org.eclipse.ui.internal.WorkbenchImages;
import org.eclipse.ui.internal.WorkbenchMessages;
import org.eclipse.ui.wizards.IWizardRegistry;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.ui.dialogs.GenericWizardRegistry;
import org.kalypso.contribs.eclipse.ui.dialogs.GenericWizardSelectionPage;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;

/**
 * @author Gernot Belger
 */
@SuppressWarnings("restriction")
public class ExportStateWizard extends Wizard
{
  public ExportStateWizard( final IStructuredSelection selection )
  {
    setWindowTitle( "Export" ); //$NON-NLS-1$
    setDefaultPageImageDescriptor( WorkbenchImages.getImageDescriptor( IWorkbenchGraphicConstants.IMG_WIZBAN_EXPORT_WIZ ) );
    setNeedsProgressMonitor( true );

    setDialogSettings( DialogSettingsUtils.getDialogSettings( WspmPdbUiPlugin.getDefault(), getClass().getName() ) );

    final String description = WorkbenchMessages.ImportExportPage_chooseExportDestination;
    final String message = WorkbenchMessages.ExportWizard_selectDestination;
    final IWizardRegistry registry = new GenericWizardRegistry( WspmPdbUiPlugin.PLUGIN_ID, "exportStateWizards" ); //$NON-NLS-1$
    final GenericWizardSelectionPage page = new GenericWizardSelectionPage( registry, selection, "export", message, description ); //$NON-NLS-1$
    addPage( page );
  }

  @Override
  public boolean performFinish( )
  {
    ((GenericWizardSelectionPage) getPages()[0]).saveWidgetValues();
    return true;
  }
}
