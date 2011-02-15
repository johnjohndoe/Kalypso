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
package org.kalypso.model.wspm.tuhh.ui.export.wspwin;

import java.io.File;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserDelegateDirectory;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultFactory;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultLengthSection;
import org.kalypso.model.wspm.tuhh.ui.export.ExportProfilesWizard;
import org.kalypso.model.wspm.tuhh.ui.export.ProfileResultExportPage;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.action.ProfileSelection;

/**
 * @author kimwerner
 */
public class PrfExportProfilesWizard extends ExportProfilesWizard
{
  private final ExportPrfFileChooserPage m_profileFileChooserPage;

  private final ProfileResultExportPage m_resultPage;

  public PrfExportProfilesWizard( final ProfileSelection selection )
  {
    super( selection );

    setDialogSettings( PluginUtilities.getDialogSettings( KalypsoModelWspmUIPlugin.getDefault(), getClass().getName() ) );

    final FileChooserDelegateDirectory dirDelegate = new FileChooserDelegateDirectory();
    m_profileFileChooserPage = new ExportPrfFileChooserPage( dirDelegate );
    m_profileFileChooserPage.setTitle( Messages.getString("PrfExportProfilesWizard_0") ); //$NON-NLS-1$
    m_profileFileChooserPage.setDescription( Messages.getString("PrfExportProfilesWizard_1") ); //$NON-NLS-1$
    m_profileFileChooserPage.setFileGroupText( Messages.getString("PrfExportProfilesWizard_2") ); //$NON-NLS-1$
    addPage( m_profileFileChooserPage );

    final IWspmResultNode results = WspmResultFactory.createResultNode( null, selection.getContainer() );
    m_resultPage = new ProfileResultExportPage( "profileResults", results ); //$NON-NLS-1$
    m_resultPage.setShowComponentChooser( false );
    addPage( m_resultPage );
  }

  @Override
  protected void exportProfiles( final IProfileFeature[] profiles, final IProgressMonitor monitor ) throws CoreException
  {
    final File exportDir = m_profileFileChooserPage.getFile();
    final String filenamePattern = m_profileFileChooserPage.getFilenamePattern();

    final WspmResultLengthSection[] results = m_resultPage.getSelectedLengthSections();

    final IPrfExporterCallback callback = new DefaultPrfExportWizardCallback( exportDir, filenamePattern, results );
    final PrfExporter prfExporter = new PrfExporter( callback );
    final IStatus export = prfExporter.export( profiles, monitor );
    if( !export.isOK() )
      throw new CoreException( export );
  }
}
