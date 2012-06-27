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
package org.kalypso.model.wspm.tuhh.ui.export.knauf;

import java.io.File;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IWorkbench;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserDelegateSave;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.KnaufCalculation;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.KnaufProfileExporter;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.export.ExportFileChooserPage;
import org.kalypso.model.wspm.tuhh.ui.export.ExportProfilesWizard;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;

/**
 * @author Dirk Kuch
 */
public class KnaufProfileExportWizard extends ExportProfilesWizard
{
  private static final String FILTER_LABEL = Messages.getString( "KnaufExportProfilesWizard_0" ); //$NON-NLS-1$

  private static final String EXTENSION = "wspr"; //$NON-NLS-1$

  private ExportFileChooserPage m_profileFileChooserPage;

  public KnaufProfileExportWizard( )
  {
    setDialogSettings( DialogSettingsUtils.getDialogSettings( KalypsoModelWspmUIPlugin.getDefault(), getClass().getName() ) );
  }

  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    super.init( workbench, selection );

    setShowResultInterpolationSettings( false );

    /* Export file */
    final FileChooserDelegateSave delegateSave = new FileChooserDelegateSave();
    delegateSave.addFilter( FILTER_LABEL, "*." + EXTENSION ); //$NON-NLS-1$

    m_profileFileChooserPage = new ExportFileChooserPage( delegateSave );
    m_profileFileChooserPage.setTitle( STR_CHOOSE_EXPORT_FILE_TITLE );
    m_profileFileChooserPage.setDescription( STR_CHOOSE_EXPORT_FILE_MESSAGE );
    m_profileFileChooserPage.setFileGroupText( STR_EXPORT_FILE_GROUP_TEXT );

    addPage( m_profileFileChooserPage );
  }

  @Override
  protected IStatus exportProfiles( final IProfileFeature[] profiles, final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      final File file = m_profileFileChooserPage.getFile();

      /* Write profiles */
      final KnaufCalculation calculation = new KnaufCalculation( profiles );

      final KnaufProfileExporter exporter = new KnaufProfileExporter( calculation, file );
      return exporter.execute( monitor );

    }
    catch( final Exception e )
    {
      throw new CoreException( new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getID(), Messages.getString("KnaufProfileExportWizard.0"), e ) ); //$NON-NLS-1$
    }
  }
}