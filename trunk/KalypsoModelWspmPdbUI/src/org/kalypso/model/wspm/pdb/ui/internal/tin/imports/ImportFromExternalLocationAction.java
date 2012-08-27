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
package org.kalypso.model.wspm.pdb.ui.internal.tin.imports;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.gml.ui.coverage.CoverageManagementWidget;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiImages;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

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
   * The coverage management widget.
   */
  private final CoverageManagementWidget m_widget;

  /**
   * The constructor.
   * 
   * @param shell
   *          The shell.
   * @param widget
   *          The coverage management widget.
   */
  public ImportFromExternalLocationAction( final Shell shell, final CoverageManagementWidget widget )
  {
    super( "H�hendaten aus externen Speicherort hinzuf�gen" );

    m_shell = shell;
    m_widget = widget;

    setImageDescriptor( WspmPdbUiImages.getImageDescriptor( WspmPdbUiImages.IMAGE.ADD_COVERAGE ) );
  }

  @Override
  public void run( )
  {
    try
    {
      // TODO: potentially slow, maybe call in operation
      final PdbImportConnectionChooserData settingsData = new PdbImportConnectionChooserData();
      final IPdbConnection connection = PdbImportConnectionChooserData.checkConnection();
      settingsData.setConnection( connection );

      final ICoverageCollection coveragesContainer = m_widget.getCoverageCollection();
      final IContainer dataContainer = m_widget.findGridFolder();

      /* Create the wizard. */
      final PdbImportCoveragesWizard wizard = new PdbImportCoveragesWizard( settingsData, coveragesContainer, dataContainer );

      /* Open the dialog. */
      final WizardDialog dialog = new WizardDialog( m_shell, wizard );
      dialog.open();
    }
    catch( final CoreException e )
    {
      StatusDialog.open( m_shell, e.getStatus(), getText() );
    }
  }
}