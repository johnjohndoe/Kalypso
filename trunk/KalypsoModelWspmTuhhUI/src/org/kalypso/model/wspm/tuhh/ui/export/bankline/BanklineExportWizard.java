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
package org.kalypso.model.wspm.tuhh.ui.export.bankline;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.gml.ui.commands.exportshape.ExportShapePage;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class BanklineExportWizard extends Wizard implements IWorkbenchWizard
{
  private final BanklineExportData m_data = new BanklineExportData();

  private ExportShapePage m_exportShapePage;

  public BanklineExportWizard( )
  {
    setWindowTitle( Messages.getString("BanklineExportWizard_0") ); //$NON-NLS-1$
    setNeedsProgressMonitor( true );

    setDialogSettings( DialogSettingsUtils.getDialogSettings( KalypsoModelWspmTuhhUIPlugin.getDefault(), getClass().getName() ) );
  }

  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    m_data.init( getDialogSettings(), selection );
  }

  @Override
  public void addPages( )
  {
    // TODO: page, that shows chosen features for export

    addPage( new BanklineExportOptionsPage( "options", m_data ) ); //$NON-NLS-1$

    /* Shape file and encoding etc. */
    final String shapeFilename = m_data.getProjectName();
    m_exportShapePage = new ExportShapePage( "exportShapePage", shapeFilename ); //$NON-NLS-1$
    addPage( m_exportShapePage );

    // TODO: page for shape attributes?
  }

  @Override
  public boolean performCancel( )
  {
    /* Save settings for next time */
    m_data.storeSettings( getDialogSettings() );

    return super.performCancel();
  }

  @Override
  public boolean performFinish( )
  {
    /* Extract data */
    m_data.setExportCrs( m_exportShapePage.getCoordinateSystem() );
    m_data.setExportCharset( m_exportShapePage.getCharset() );
    m_data.setWritePrj( m_exportShapePage.isWritePrj() );
    m_data.setExportShapeBase( m_exportShapePage.getShapeFileBase() );

    /* Save settings for next time */
    m_data.storeSettings( getDialogSettings() );

    final BanklineExportOperation operation = new BanklineExportOperation( m_data );
    final IStatus status = RunnableContextHelper.execute( getContainer(), true, true, operation );
    StatusDialog.open( getShell(), status, getWindowTitle() );

    return !status.matches( IStatus.ERROR );
  }
}