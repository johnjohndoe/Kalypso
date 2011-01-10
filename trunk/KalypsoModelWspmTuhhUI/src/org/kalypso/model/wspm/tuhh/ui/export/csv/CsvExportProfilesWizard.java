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
package org.kalypso.model.wspm.tuhh.ui.export.csv;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.FileChooserDelegateSave;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.core.profile.CsvSink;
import org.kalypso.model.wspm.tuhh.core.profile.export.IProfileExportColumn;
import org.kalypso.model.wspm.tuhh.core.profile.export.PatternReplacementColumn;
import org.kalypso.model.wspm.tuhh.core.profile.export.ResultColumn;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultFactory;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultLengthSectionColumn;
import org.kalypso.model.wspm.tuhh.ui.export.ExportFileChooserPage;
import org.kalypso.model.wspm.tuhh.ui.export.ExportProfilesWizard;
import org.kalypso.model.wspm.tuhh.ui.export.ProfileResultExportPage;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.action.ProfileSelection;
import org.kalypso.observation.result.IComponent;

/**
 * @author kimwerner
 */
public class CsvExportProfilesWizard extends ExportProfilesWizard
{
  private static final String FILTER_LABEL = Messages.getString( "CsvExportProfilesWizard_0" ); //$NON-NLS-1$

  private static final String EXTENSION = "csv"; //$NON-NLS-1$

  private final ExportFileChooserPage m_profileFileChooserPage;

  private final ProfileResultExportPage m_resultPage;

  public CsvExportProfilesWizard( final ProfileSelection selection )
  {
    super( selection );

    setDialogSettings( PluginUtilities.getDialogSettings( KalypsoModelWspmUIPlugin.getDefault(), getClass().getName() ) );

    final IWspmResultNode results = WspmResultFactory.createResultNode( null, selection.getContainer() );

    m_resultPage = new ProfileResultExportPage( "profileResults", results ); //$NON-NLS-1$
    addPage( m_resultPage );

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
    final IComponent[] components = getComponents( profiles );
    final WspmResultLengthSectionColumn[] lsColumns = m_resultPage.getSelectedColumns();

    final IProfileExportColumn[] columns = createColumns( components, lsColumns );

    final File file = m_profileFileChooserPage.getFile();

    final CsvSink csvSink = new CsvSink( columns );

    csvSink.export( profiles, file, monitor );
  }

  private IProfileExportColumn[] createColumns( final IComponent[] components, final WspmResultLengthSectionColumn[] lsColumns )
  {
    final Collection<IProfileExportColumn> columns = new ArrayList<IProfileExportColumn>();

    // FIXME: theses columns should be configurable by the user
    columns.add( new PatternReplacementColumn( "Station", "<Station>" ) ); //$NON-NLS-2$
    columns.add( new PatternReplacementColumn( "Name", "<Name>" ) ); //$NON-NLS-2$
    columns.add( new PatternReplacementColumn( "Description", "<Description>" ) ); //$NON-NLS-2$
    columns.add( new PatternReplacementColumn( "Comment", "<Comment>" ) ); //$NON-NLS-2$

    // FIXME: these columns should be configurable by the user (at least, 'all or nothing')
    for( final IComponent comp : components )
    {
      final String pattern = String.format( "<Component:%s>", comp.getId() );
      columns.add( new PatternReplacementColumn( comp.getName(), pattern ) );
    }

    for( final WspmResultLengthSectionColumn ls : lsColumns )
      columns.add( new ResultColumn( ls ) );

    return columns.toArray( new IProfileExportColumn[columns.size()] );
  }

  private final IComponent[] getComponents( final IProfileFeature[] profiles )
  {
    final Set<IComponent> profCompSet = new HashSet<IComponent>();
    for( final IProfileFeature profileFeature : profiles )
    {
      final IProfil profil = profileFeature.getProfil();
      for( final IComponent component : profil.getPointProperties() )
        profCompSet.add( component );
    }
    return profCompSet.toArray( new IComponent[] {} );
  }
}
