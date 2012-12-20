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
import java.util.Locale;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
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
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;

/**
 * @author Gernot Belger
 */
public class TrippleExportProfilesWizard extends ExportProfilesWizard
{
  /** Tripple is ',' separated */
  private static final String TOKEN_SEPARATOR = ","; //$NON-NLS-1$

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
  protected IStatus exportProfiles( final IProfileFeature[] profiles, final IProgressMonitor monitor ) throws CoreException
  {
    final File file = m_profileFileChooserPage.getFile();

    /* Write profiles */
    final AbstractCsvWriter csvSink = createWriter();
    csvSink.export( profiles, file, monitor );

    return Status.OK_STATUS;
  }

  private AbstractCsvWriter createWriter( )
  {
    final IComponent rwComp = ComponentUtilities.getFeatureComponent( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
    final IComponent hwComp = ComponentUtilities.getFeatureComponent( IWspmConstants.POINT_PROPERTY_HOCHWERT );
    final IComponent heightComp = ComponentUtilities.getFeatureComponent( IWspmConstants.POINT_PROPERTY_HOEHE );

    final String rwPattern = String.format( "<Component:%s>", IWspmConstants.POINT_PROPERTY_RECHTSWERT ); //$NON-NLS-1$
    final String hwPattern = String.format( "<Component:%s>", IWspmConstants.POINT_PROPERTY_HOCHWERT ); //$NON-NLS-1$
    final String heightPattern = String.format( "<Component:%s>", IWspmConstants.POINT_PROPERTY_HOEHE ); //$NON-NLS-1$

    final PatternReplacementColumn station = new PatternReplacementColumn( Messages.getString( "CsvExportColumnsPage_5" ), "<Station>" ); //$NON-NLS-1$//$NON-NLS-2$
    final IProfileExportColumn rw = new PatternReplacementColumn( rwComp.getName(), rwPattern );
    final IProfileExportColumn hw = new PatternReplacementColumn( hwComp.getName(), hwPattern );
    final IProfileExportColumn height = new PatternReplacementColumn( heightComp.getName(), heightPattern );
    final IProfileExportColumn[] columns = new IProfileExportColumn[] { station, rw, hw, height };

    /* Using US Local, in order to format numbers with '.', which is standard for tripple */
    final Locale usLocale = Locale.US;

    for( final IProfileExportColumn column : columns )
      column.setLocale( usLocale );

    return new CsvPointsWriter( columns, TOKEN_SEPARATOR );
  }
}