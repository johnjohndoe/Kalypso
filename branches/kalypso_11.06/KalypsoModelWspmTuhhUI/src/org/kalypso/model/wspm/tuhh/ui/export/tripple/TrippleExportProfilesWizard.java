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
package org.kalypso.model.wspm.tuhh.ui.export.tripple;

import java.io.File;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IWorkbench;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserDelegateSave;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.core.profile.export.AbstractCsvWriter;
import org.kalypso.model.wspm.tuhh.core.profile.export.CsvPointsWriter;
import org.kalypso.model.wspm.tuhh.core.profile.export.IProfileExportColumn;
import org.kalypso.model.wspm.tuhh.core.profile.export.PatternReplacementColumn;
import org.kalypso.model.wspm.tuhh.ui.export.ExportFileChooserPage;
import org.kalypso.model.wspm.tuhh.ui.export.ExportProfilesWizard;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;

/**
 * @author kimwerner
 */
public class TrippleExportProfilesWizard extends ExportProfilesWizard
{
  private static final String FILTER_LABEL = Messages.getString( "TrippleExportProfilesWizard_0" ); //$NON-NLS-1$

  private static final String EXTENSION = "txt"; //$NON-NLS-1$

  private ExportFileChooserPage m_profileFileChooserPage;

  public TrippleExportProfilesWizard( )
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
  protected void exportProfiles( final IProfileFeature[] profiles, final IProgressMonitor monitor ) throws CoreException
  {
    final File file = m_profileFileChooserPage.getFile();

    /* Write profiles */
    final AbstractCsvWriter csvSink = createWriter();
    csvSink.export( profiles, file, monitor );
  }

  private AbstractCsvWriter createWriter( )
  {
    final String rwPattern = String.format( "<Component:%s>", IWspmConstants.POINT_PROPERTY_RECHTSWERT ); //$NON-NLS-1$
    final String hwPattern = String.format( "<Component:%s>", IWspmConstants.POINT_PROPERTY_HOCHWERT ); //$NON-NLS-1$
    final String heightPattern = String.format( "<Component:%s>", IWspmConstants.POINT_PROPERTY_HOEHE ); //$NON-NLS-1$

    final IProfileExportColumn rw = new PatternReplacementColumn( "rw", rwPattern );
    final IProfileExportColumn hw = new PatternReplacementColumn( "hw", hwPattern );
    final IProfileExportColumn height = new PatternReplacementColumn( "height", heightPattern );
    final IProfileExportColumn[] columns = new IProfileExportColumn[] { rw, hw, height };

    return new CsvPointsWriter( columns );
  }
}