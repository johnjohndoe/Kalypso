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
package org.kalypso.model.wspm.tuhh.ui.wizards;

import java.io.File;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.model.wspm.tuhh.core.wspwin.WspWinExportData;
import org.kalypso.model.wspm.tuhh.core.wspwin.WspWinExportGmlData;
import org.kalypso.model.wspm.tuhh.core.wspwin.WspWinExportGmlOperation;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * Exports WspWin projects from selected Wspm-GML elements such as water bodies or reaches.
 * 
 * @author Gernot Belger
 */
public class WspWinExportGmlWizard extends Wizard implements IWorkbenchWizard
{
  private final WspWinExportGmlData m_data = new WspWinExportGmlData();

  private WspWinExportDestinationPage m_destinationPage;

  /**
   * Creates a wizard for exporting workspace resources into a wspwin project.
   */
  public WspWinExportGmlWizard( )
  {
    final IDialogSettings section = DialogSettingsUtils.getDialogSettings( KalypsoModelWspmTuhhUIPlugin.getDefault(), WspWinExportData.SETTINGS_SECTION_NAME );
    setDialogSettings( section );

    setWindowTitle( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizards.WspWinExportProjectWizard.1" ) ); //$NON-NLS-1$

    setForcePreviousAndNextButtons( false );
    setNeedsProgressMonitor( true );
  }

  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    m_data.setSelection( selection );

    m_data.loadSettings( getDialogSettings() );
  }

  @Override
  public void addPages( )
  {
    m_destinationPage = new WspWinExportDestinationPage( "destinationSelection", m_data ); //$NON-NLS-1$
    addPage( m_destinationPage );
  }

  @Override
  public boolean performCancel( )
  {
    m_data.storeSettings( getDialogSettings() );
    return super.performCancel();
  }

  @Override
  public boolean performFinish( )
  {
    m_data.storeSettings( getDialogSettings() );

    final Shell shell = getContainer().getShell();

    final boolean overwriteExisting = m_data.getOverwriteExisting();
    final File outputDir = m_data.getOutputDir();
    if( !overwriteExisting && outputDir.isDirectory() )
    {
      final String msg = Messages.getString( "WspWinExportGmlWizard.0" ); //$NON-NLS-1$
      if( !MessageDialog.openConfirm( shell, getWindowTitle(), msg ) )
        return false;
    }

    final WspWinExportGmlOperation operation = new WspWinExportGmlOperation( m_data );

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, true, operation );
    if( !status.isOK() )
      StatusUtilities.printStackTraces( status );
    ErrorDialog.openError( shell, getWindowTitle(), Messages.getString( "org.kalypso.model.wspm.tuhh.ui.wizards.WspWinExportProjectWizard.4" ), status ); //$NON-NLS-1$ //$NON-NLS-2$

    return status.isOK();
  }

}
