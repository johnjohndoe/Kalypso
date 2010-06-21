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

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.model.Util;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.wizard.IKalypsoDataImportWizard;
import org.kalypso.ui.wizards.i18n.Messages;
import org.kalypso.ui.wizards.results.filters.DocumentResultViewerFilter;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

import de.renew.workflow.connector.cases.CaseHandlingSourceProvider;
import de.renew.workflow.connector.cases.ICaseDataProvider;

/**
 * Wizard to manage result data for Kalypso 1D/2D.
 * 
 * allows to delete or reinterpret the existing results
 *
 * @author Thomas Jung
 */
public class ResultManager1d2dWizard extends Wizard implements IKalypsoDataImportWizard
{
  private final static String PAGE_SELECT_RESULTS_NAME = "selectResults"; //$NON-NLS-1$

  private IKalypsoLayerModell m_modell;

  private IScenarioResultMeta m_resultModel;

  private ICommandTarget m_commandTarget;

  private ICaseDataProvider<IFeatureWrapper2> m_modelProvider;

  private ResultManager1d2dWizardPage m_selectResultWizardPage;

  private IGeoLog m_geoLog;

  public ResultManager1d2dWizard( )
  {
    setWindowTitle( Messages.getString("org.kalypso.ui.wizards.results.ResultManager1d2dWizard.1") ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    final DocumentResultViewerFilter resultFilter = new DocumentResultViewerFilter();
    final Result1d2dMetaComparator resultComparator = new Result1d2dMetaComparator();

    m_selectResultWizardPage = new ResultManager1d2dWizardPage( PAGE_SELECT_RESULTS_NAME, Messages.getString("org.kalypso.ui.wizards.results.ResultManager1d2dWizard.2"), null, resultFilter, resultComparator, null, m_geoLog ); //$NON-NLS-1$
    m_selectResultWizardPage.setResultMeta( m_resultModel );
    m_selectResultWizardPage.setCommandTarget( m_commandTarget );
    m_selectResultWizardPage.setMapModel( m_modell );
    m_selectResultWizardPage.setCaseDataProvidet( m_modelProvider );

    addPage( m_selectResultWizardPage );
  }

  /**
   * @see org.kalypso.ui.wizard.IKalypsoDataImportWizard#setCommandTarget(org.kalypso.commons.command.ICommandTarget)
   */
  @Override
  public void setCommandTarget( final ICommandTarget commandTarget )
  {
    m_commandTarget = commandTarget;
  }

  /**
   * @see org.kalypso.ui.wizard.IKalypsoDataImportWizard#setMapModel(org.kalypso.ogc.gml.IKalypsoLayerModell)
   */
  @Override
  public void setMapModel( final IKalypsoLayerModell modell )
  {
    m_modell = modell;
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  @Override
  @SuppressWarnings("unchecked")
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );
    m_modelProvider = (ICaseDataProvider<IFeatureWrapper2>) context.getVariable( CaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
    try
    {
      // Sometimes there is a NPE here... maybe wait until the models are loaded?
      m_resultModel = m_modelProvider.getModel( IScenarioResultMeta.class.getName(), IScenarioResultMeta.class );
    }
    catch( final CoreException e )
    {
      Kalypso1d2dProjectPlugin.getDefault().getLog().log( e.getStatus() );
      ErrorDialog.openError( shell, Messages.getString("org.kalypso.ui.wizards.results.ResultManager1d2dWizard.3"), Messages.getString("org.kalypso.ui.wizards.results.ResultManager1d2dWizard.4"), e.getStatus() ); //$NON-NLS-1$ //$NON-NLS-2$
    }
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    try
    {
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
