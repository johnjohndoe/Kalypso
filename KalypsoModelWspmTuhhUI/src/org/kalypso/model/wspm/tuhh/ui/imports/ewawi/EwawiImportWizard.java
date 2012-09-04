/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.ui.imports.ewawi;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.ui.imports.WspmTuhhProjectSelection;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;

/**
 * @author Holger Albert
 */
public class EwawiImportWizard extends Wizard implements IWorkbenchWizard
{
  private CommandableWorkspace m_workspace;

  private TuhhWspmProject m_targetProject;

  private EwawiImportData m_data;

  public EwawiImportWizard( )
  {
    m_workspace = null;
    m_targetProject = null;
    m_data = null;

    setDialogSettings( DialogSettingsUtils.getDialogSettings( KalypsoModelWspmTuhhUIPlugin.getDefault(), "ewawiImportWizard" ) ); //$NON-NLS-1$
    setWindowTitle( "EWAWI+ Import" );
    setNeedsProgressMonitor( true );
  }

  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    final WspmTuhhProjectSelection projectSelection = new WspmTuhhProjectSelection( selection );
    if( !projectSelection.hasProject() )
      throw new IllegalArgumentException( Messages.getString( "ImportWProfHandler_1" ) ); //$NON-NLS-1$

    m_workspace = projectSelection.getWorkspace();
    m_targetProject = projectSelection.getProject();
    m_data = new EwawiImportData();
    m_data.init( getDialogSettings() );
  }

  @Override
  public void addPages( )
  {
    addPage( new EwawiImportFilesPage( m_data ) );
  }

  @Override
  public boolean performFinish( )
  {
    /* Save the dialog settings. */
    m_data.storeSettings( getDialogSettings() );

    final EwawiImportOperation operation = new EwawiImportOperation( m_workspace, m_targetProject, m_data );
    final IStatus result = RunnableContextHelper.execute( getContainer(), true, true, operation );
    if( !result.isOK() )
      StatusDialog.open( getShell(), result, getWindowTitle() );

    return !result.matches( IStatus.ERROR );
  }
}