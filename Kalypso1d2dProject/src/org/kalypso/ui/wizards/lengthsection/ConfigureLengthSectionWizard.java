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
package org.kalypso.ui.wizards.lengthsection;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.lengthsection.LengthSectionParameters;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ui.wizards.i18n.Messages;
import org.kalypso.ui.wizards.results.Result1d2dMetaComparator;
import org.kalypso.ui.wizards.results.SelectResultWizardPage;
import org.kalypso.ui.wizards.results.filters.DocumentResultViewerFilter;

/**
 * Wizard to show length sections to the chart view.
 *
 * @author Thomas Jung
 */
public class ConfigureLengthSectionWizard extends Wizard
{
  private final static String PAGE_SELECT_RESULTS_NAME = "selectResults"; //$NON-NLS-1$

  private final static String PAGE_GENERATE_LENGTH_SECTION_NAME = "generateLengthSection"; //$NON-NLS-1$

  private final IScenarioResultMeta m_resultModel;

  final IFolder m_scenarioFolder;

  private IFile m_selectedResultFile;

  private final IMapPanel m_mapPanel;

  ConfigureLengthSectionWizardPage m_lengthSectionPage;

  public ConfigureLengthSectionWizard( final IFolder scenarioFolder, final IScenarioResultMeta resultModel, final IMapPanel mapPanel )
  {
    m_scenarioFolder = scenarioFolder;
    m_resultModel = resultModel;
    m_mapPanel = mapPanel;
    setWindowTitle( Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.Title" ) ); //$NON-NLS-1$

    setNeedsProgressMonitor( true );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    m_lengthSectionPage = new ConfigureLengthSectionWizardPage( PAGE_GENERATE_LENGTH_SECTION_NAME, Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.3" ), null, m_mapPanel ); //$NON-NLS-1$
    // select time step page
    final DocumentResultViewerFilter resultFilter = new DocumentResultViewerFilter();
    final Result1d2dMetaComparator comparator = new Result1d2dMetaComparator();

    final SelectResultWizardPage selectResultWizardPage = new SelectResultWizardPage( PAGE_SELECT_RESULTS_NAME, Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.4" ), null, resultFilter, comparator, null, null ); //$NON-NLS-1$

    selectResultWizardPage.setResultMeta( m_resultModel );

    addPage( m_lengthSectionPage );
    addPage( selectResultWizardPage );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    final ConfigureLengthSectionWizardPage parameterPage = (ConfigureLengthSectionWizardPage) getPage( PAGE_GENERATE_LENGTH_SECTION_NAME );
    final LengthSectionParameters lengthSectionParameters = parameterPage.getLengthSectionParameters();
    final SelectResultWizardPage resultPage = (SelectResultWizardPage) getPage( PAGE_SELECT_RESULTS_NAME );
    final IResultMeta[] results = resultPage.getSelectedResults();

    if( results.length == 0 )
    {
      MessageDialog.openInformation( getShell(), Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.5" ), Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.6" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      return false;
    }

    /* Start */
    final ICoreRunnableWithProgress op = new Generate2dSectionRunnable( results, lengthSectionParameters,parameterPage.isKmValues(), m_scenarioFolder  );

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, op );
    if( !status.isOK() )
      KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
    ErrorDialog.openError( getShell(), getWindowTitle(), Messages.getString( "org.kalypso.ui.wizards.lengthsection.ConfigureLengthSectionWizard.26" ), status ); //$NON-NLS-1$

    if( status.matches( IStatus.ERROR ) || status.matches( IStatus.WARNING ) )
      return false;
    else
      return true;

  }

  public IFile getSelection( )
  {
    return m_selectedResultFile;

  }
}
