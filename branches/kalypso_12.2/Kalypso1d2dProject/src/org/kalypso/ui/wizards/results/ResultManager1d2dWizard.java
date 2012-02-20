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
package org.kalypso.ui.wizards.results;

import org.eclipse.jface.wizard.Wizard;
import org.kalypso.afgui.model.IModel;
import org.kalypso.afgui.model.Util;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.wizards.i18n.Messages;
import org.kalypso.ui.wizards.results.filters.DocumentResultViewerFilter;
import org.kalypso.util.command.JobExclusiveCommandTarget;

import de.renew.workflow.connector.cases.ICaseDataProvider;

/**
 * Wizard to manage result data for Kalypso 1D/2D.
 * 
 * allows to delete or reinterpret the existing results
 *
 * @author Thomas Jung
 */
public class ResultManager1d2dWizard extends Wizard
{
  private final static String PAGE_SELECT_RESULTS_NAME = "selectResults"; //$NON-NLS-1$

  private final IKalypsoLayerModell m_modell;

  private final IScenarioResultMeta m_resultModel;

  private final ICommandTarget m_commandTarget;

  private final ICaseDataProvider<IModel> m_modelProvider;

  private IGeoLog m_geoLog;

  public ResultManager1d2dWizard( final IKalypsoLayerModell mapModel, final JobExclusiveCommandTarget commandTarget, final IScenarioResultMeta resultModel, final ICaseDataProvider<IModel> modelProvider )
  {
    m_modell = mapModel;
    m_commandTarget = commandTarget;
    m_resultModel = resultModel;
    m_modelProvider = modelProvider;

    setWindowTitle( Messages.getString("org.kalypso.ui.wizards.results.ResultManager1d2dWizard.1") ); //$NON-NLS-1$
    setNeedsProgressMonitor( true );
  }

  @Override
  public void addPages( )
  {
    final DocumentResultViewerFilter resultFilter = new DocumentResultViewerFilter();
    final Result1d2dMetaComparator resultComparator = new Result1d2dMetaComparator();

    final SelectResultWizardPage selectResultWizardPage = new ResultManager1d2dWizardPage( PAGE_SELECT_RESULTS_NAME, Messages.getString("org.kalypso.ui.wizards.results.ResultManager1d2dWizard.2"), null, resultFilter, resultComparator, null, m_geoLog ); //$NON-NLS-1$
    selectResultWizardPage.setResultMeta( m_resultModel );

    selectResultWizardPage.addAction( new DeleteResultAction( selectResultWizardPage, m_commandTarget, m_modell ) );
    selectResultWizardPage.addAction( new ReevaluateResultAction( selectResultWizardPage, m_commandTarget, m_modell, m_modelProvider ) );
    selectResultWizardPage.addAction( new ImportResultAction( selectResultWizardPage, m_modelProvider ) );

    addPage( selectResultWizardPage );
  }

  @Override
  public boolean performFinish( )
  {
    try
    {
      // FIXME: we always make the pool dirty and safe the workspace... is this ok?
      // FIXME: what happens with cancel?

      final EmptyCommand command = new EmptyCommand( "You are dirty now, pool!", false ); //$NON-NLS-1$
      final CommandableWorkspace commandableWorkspace = Util.getCommandableWorkspace( IScenarioResultMeta.class );
      commandableWorkspace.postCommand( command );
      m_modelProvider.saveModel( IScenarioResultMeta.class.getName(), null );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    return true;
  }
}
