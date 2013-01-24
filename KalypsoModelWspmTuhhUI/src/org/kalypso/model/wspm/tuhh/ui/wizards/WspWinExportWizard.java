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
import java.util.Iterator;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IExportWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.model.wspm.tuhh.core.wspwin.WspWinExporter;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author thuel2
 */

public class WspWinExportWizard extends Wizard implements IExportWizard
{
  private IStructuredSelection m_selection;

  private WspWinExportPage m_wspWinExportPage;

  /**
   * Creates a wizard for exporting workspace resources into a wspwin project.
   */
  public WspWinExportWizard( )
  {
    final IDialogSettings pluginSettings = KalypsoModelWspmTuhhUIPlugin.getDefault().getDialogSettings();
    final IDialogSettings section = pluginSettings.getSection( "WspWinExportWizard" );//$NON-NLS-1$
    if( section != null )
      setDialogSettings( section );
    else
      setDialogSettings( pluginSettings.addNewSection( "WspWinExportWizard" ) );//$NON-NLS-1$

    setForcePreviousAndNextButtons( false );
  }

  /*
   * (non-Javadoc) Method declared on IWizard.
   */
  @Override
  public void addPages( )
  {
    m_wspWinExportPage = new WspWinExportPage( m_selection );
    addPage( m_wspWinExportPage );
  }

  /*
   * (non-Javadoc) Method declared on IWorkbenchWizard.
   */
  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection currentSelection )
  {
    m_selection = currentSelection;
    setWindowTitle( Messages.getString("org.kalypso.model.wspm.tuhh.ui.wizards.WspWinExportWizard.0") ); //$NON-NLS-1$
    setNeedsProgressMonitor( true );
  }

  /*
   * (non-Javadoc) Method declared on IWizard.
   */
  @Override
  public boolean performFinish( )
  {
    m_wspWinExportPage.saveWidgetValues();
    final Shell shell = getContainer().getShell();

    // get model.gml (wspmTuhhModel.gml)
    final Iterator<IResource> modelGml = m_wspWinExportPage.getSelectedResourcesIterator();

    // get destination path (wspwin path, not wspwin project path)
    final File wspwinDir = m_wspWinExportPage.getDestinationDirectory();

    // set visitor loose
    final WorkspaceModifyOperation operation = new WorkspaceModifyOperation()
    {
      @Override
      protected void execute( final IProgressMonitor monitor ) throws CoreException
      {
        monitor.beginTask( Messages.getString("org.kalypso.model.wspm.tuhh.ui.wizards.WspWinExportWizard.1"), 100 ); //$NON-NLS-1$

        try
        {
          monitor.subTask( Messages.getString("org.kalypso.model.wspm.tuhh.ui.wizards.WspWinExportWizard.2") ); //$NON-NLS-1$
          final IStatus status = WspWinExporter.exportWspmProject( modelGml, wspwinDir, new SubProgressMonitor( monitor, 90 ) );
          if( !status.isOK() )
            throw new CoreException( status );
        }
        finally
        {
          monitor.done();
        }
      }
    };

    final IStatus status = RunnableContextHelper.execute( getContainer(), false, false, operation );
    if( !status.isOK() )
      StatusUtilities.printStackTraces( status );
    ErrorDialog.openError( shell, Messages.getString("org.kalypso.model.wspm.tuhh.ui.wizards.WspWinExportWizard.3"), Messages.getString("org.kalypso.model.wspm.tuhh.ui.wizards.WspWinExportWizard.4"), status ); //$NON-NLS-1$ //$NON-NLS-2$

    return status.isOK();
  }

}
