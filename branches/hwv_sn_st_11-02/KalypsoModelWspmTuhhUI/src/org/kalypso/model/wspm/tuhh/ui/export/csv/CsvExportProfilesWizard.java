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
package org.kalypso.model.wspm.tuhh.ui.export.csv;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserDelegateSave;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.core.profile.export.AbstractCsvWriter;
import org.kalypso.model.wspm.tuhh.core.profile.export.CsvPointsWriter;
import org.kalypso.model.wspm.tuhh.core.profile.export.CsvProfilesWriter;
import org.kalypso.model.wspm.tuhh.core.profile.export.IProfileExportColumn;
import org.kalypso.model.wspm.tuhh.core.profile.export.PatternReplacementColumn;
import org.kalypso.model.wspm.tuhh.core.profile.export.ProfileExportUtils;
import org.kalypso.model.wspm.tuhh.core.profile.export.ResultProfileInterpolator;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResult;
import org.kalypso.model.wspm.tuhh.ui.export.ExportFileChooserPage;
import org.kalypso.model.wspm.tuhh.ui.export.ExportProfilesWizard;
import org.kalypso.model.wspm.tuhh.ui.export.csv.CsvExportColumnsPage.OUTPUT_TYPE;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.action.ProfileSelection;
import org.kalypso.model.wspm.ui.profil.wizard.results.IResultInterpolationSettings;
import org.kalypso.observation.result.IComponent;

/**
 * @author kimwerner
 */
public class CsvExportProfilesWizard extends ExportProfilesWizard
{
  private static final String FILTER_LABEL = Messages.getString( "CsvExportProfilesWizard_0" ); //$NON-NLS-1$

  private static final String EXTENSION = "csv"; //$NON-NLS-1$

  private final ExportFileChooserPage m_profileFileChooserPage;

  private final CsvExportColumnsPage m_columnsPage;

  public CsvExportProfilesWizard( final ProfileSelection selection )
  {
    super( selection );

    setDialogSettings( PluginUtilities.getDialogSettings( KalypsoModelWspmUIPlugin.getDefault(), getClass().getName() ) );

    setShowResultInterpolationSettings( true );

    /* Export file */
    final FileChooserDelegateSave delegateSave = new FileChooserDelegateSave();
    delegateSave.addFilter( FILTER_LABEL, "*." + EXTENSION ); //$NON-NLS-1$

    m_profileFileChooserPage = new ExportFileChooserPage( delegateSave );
    m_profileFileChooserPage.setTitle( STR_CHOOSE_EXPORT_FILE_TITLE );
    m_profileFileChooserPage.setDescription( STR_CHOOSE_EXPORT_FILE_MESSAGE );
    m_profileFileChooserPage.setFileGroupText( STR_EXPORT_FILE_GROUP_TEXT );

    addPage( m_profileFileChooserPage );

    m_columnsPage = new CsvExportColumnsPage( selection );
    addPage( m_columnsPage );
  }

  @Override
  protected void exportProfiles( final IProfileFeature[] profiles, final IProgressMonitor monitor ) throws CoreException
  {
    m_columnsPage.saveConfiguration();

    final File file = m_profileFileChooserPage.getFile();
    final IProfileExportColumn[] userDefinedColumns = m_columnsPage.getExportColumns();
    final OUTPUT_TYPE type = m_columnsPage.getType();

    final IWspmResult[] results = ProfileExportUtils.findResults( profiles, userDefinedColumns );

    final IResultInterpolationSettings resultInterpolationSettings = getResultInterpolationSettings();
    final boolean doAdd = resultInterpolationSettings.shouldAddInterpolatedProfiles();
    final boolean interpolateForeland = resultInterpolationSettings.shouldInterpolateForland();

    final ResultProfileInterpolator interpolator = new ResultProfileInterpolator( profiles, results, doAdd, interpolateForeland );
    final IProfileFeature[] interpolatedProfiles = interpolator.execute();

    final AbstractCsvWriter csvSink = createWriter( interpolatedProfiles, userDefinedColumns, type );
    csvSink.export( interpolatedProfiles, file, monitor );
  }

  private AbstractCsvWriter createWriter( final IProfileFeature[] profiles, final IProfileExportColumn[] userDefinedColumns, final OUTPUT_TYPE type )
  {
    final IComponent[] components = ProfileExportUtils.getComponents( profiles );

    final IProfileExportColumn[] columns = createColumns( userDefinedColumns, components, type );

    switch( type )
    {
      case point:
        return new CsvPointsWriter( columns );

      case profiles:
        return new CsvProfilesWriter( columns );
    }

    throw new IllegalArgumentException();
  }

  private IProfileExportColumn[] createColumns( final IProfileExportColumn[] userDefinedColumns, final IComponent[] components, final OUTPUT_TYPE type )
  {
    final Collection<IProfileExportColumn> columns = new ArrayList<IProfileExportColumn>();

    for( final IProfileExportColumn column : userDefinedColumns )
      columns.add( column );

    if( type == OUTPUT_TYPE.point )
    {
      // FIXME: these columns should be configurable by the user (at least, 'all or nothing')
      for( final IComponent comp : components )
      {
        final String pattern = String.format( "<Component:%s>", comp.getId() ); //$NON-NLS-1$
        columns.add( new PatternReplacementColumn( comp.getName(), pattern ) );
      }
    }

    return columns.toArray( new IProfileExportColumn[columns.size()] );
  }
}
