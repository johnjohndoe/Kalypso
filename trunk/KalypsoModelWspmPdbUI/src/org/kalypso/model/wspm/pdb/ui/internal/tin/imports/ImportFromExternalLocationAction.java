/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.pdb.ui.internal.tin.imports;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.ui.internal.PdbUiUtils;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiImages;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.content.IConnectionViewer;
import org.kalypso.model.wspm.pdb.ui.internal.tin.PdbImportConnectionChooserData;

/**
 * @author Holger Albert
 */
public class ImportFromExternalLocationAction extends Action
{
  /**
   * The shell.
   */
  private final Shell m_shell;

  /**
   * The constructor.
   * 
   * @param shell
   *          The shell.
   */
  public ImportFromExternalLocationAction( final Shell shell )
  {
    super( "Höhendaten aus externen Speicherort hinzufügen" );

    m_shell = shell;

    setImageDescriptor( WspmPdbUiImages.getImageDescriptor( WspmPdbUiImages.IMAGE.ADD_COVERAGE ) );
  }

  @Override
  public void run( )
  {
    try
    {
      // TODO: potentially slow, maybe call in operation
      final PdbImportConnectionChooserData settingsData = new PdbImportConnectionChooserData();
      final IPdbConnection connection = checkConnection();
      settingsData.setConnection( connection );

      /* Create the wizard. */
      final PdbImportCoveragesWizard wizard = new PdbImportCoveragesWizard( settingsData );

      /* Open the dialog. */
      final WizardDialog dialog = new WizardDialog( m_shell, wizard );
      dialog.open();
    }
    catch( final CoreException e )
    {
      StatusDialog.open( m_shell, e.getStatus(), getText() );
    }
  }

  private IPdbConnection checkConnection( ) throws CoreException
  {
    /* Get the active workbench window. */
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();

    /* If there is a viewer, we are within the PDB perspective. */
    final IConnectionViewer connectionViewer = PdbUiUtils.getConnectionViewer( window );
    if( connectionViewer == null )
      return null;

    /* We are within the PDB perspective. */
    final IPdbConnection connection = PdbUiUtils.getConnection( window );
    if( connection == null )
      throw new CoreException( new Status( IStatus.WARNING, WspmPdbUiPlugin.PLUGIN_ID, "Es besteht keine Verbindung zur Datenbank." ) );

    return connection;
  }
}