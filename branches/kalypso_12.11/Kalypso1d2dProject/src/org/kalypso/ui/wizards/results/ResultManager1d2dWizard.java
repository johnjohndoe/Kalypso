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
package org.kalypso.ui.wizards.results;

import org.eclipse.jface.wizard.Wizard;
import org.kalypso.afgui.model.Util;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.ui.geolog.GeoLog;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.wizards.results.filters.DocumentResultViewerFilter;
import org.kalypso.util.command.JobExclusiveCommandTarget;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * Wizard to manage result data for Kalypso 1D/2D.
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

  private final IScenarioDataProvider m_modelProvider;

  private final IGeoLog m_geoLog;

  public ResultManager1d2dWizard( final IKalypsoLayerModell mapModel, final JobExclusiveCommandTarget commandTarget, final IScenarioResultMeta resultModel, final IScenarioDataProvider modelProvider )
  {
    m_modell = mapModel;
    m_commandTarget = commandTarget;
    m_resultModel = resultModel;
    m_modelProvider = modelProvider;

    setWindowTitle( Messages.getString( "org.kalypso.ui.wizards.results.ResultManager1d2dWizard.1" ) ); //$NON-NLS-1$
    setNeedsProgressMonitor( true );

    // FIXME: this log is never used and/or displayed to the user...!
    m_geoLog = createLog();
  }

  private IGeoLog createLog( )
  {
    try
    {
      return new GeoLog( Kalypso1d2dProjectPlugin.getDefault().getLog() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  @Override
  public void addPages( )
  {
    final String title = Messages.getString( "org.kalypso.ui.wizards.results.ResultManager1d2dWizard.2" ); //$NON-NLS-1$

    final SelectResultData data = new SelectResultData( m_resultModel );

    final SelectResultWizardPage selectResultWizardPage = new SelectResultWizardPage( PAGE_SELECT_RESULTS_NAME, title, data );
    selectResultWizardPage.setDescription( "Selektieren Sie die zu bearbeitenden Ergebnisse und w‰hlen Sie ein Werkzeug aus der Werkzeugleiste." );

    selectResultWizardPage.setFilter( new DocumentResultViewerFilter() );

    selectResultWizardPage.addAction( new DeleteResultAction( selectResultWizardPage, m_commandTarget, m_modell ) );
    selectResultWizardPage.addAction( new ReevaluateResultAction( selectResultWizardPage, m_commandTarget, m_modell, m_modelProvider, m_geoLog ) );
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