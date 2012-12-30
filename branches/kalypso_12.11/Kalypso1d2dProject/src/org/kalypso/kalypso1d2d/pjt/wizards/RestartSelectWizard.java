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
package org.kalypso.kalypso1d2d.pjt.wizards;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.model.ICommandPoster;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.IRestartInfo;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.StepResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ui.wizards.results.Result1d2dMetaComparator;
import org.kalypso.ui.wizards.results.filters.DocumentResultViewerFilter;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Dejan Antanaskovic
 */
public class RestartSelectWizard extends Wizard
{
  private RestartSelectWizardPage1 m_restartSelectWizardPage1;

  private RestartSelectWizardPage2 m_restartSelectWizardPage2;

  private IScenarioResultMeta m_resultModel;

  private final IScenarioDataProvider m_modelProvider;

  private final IControlModel1D2D m_controlModel;

  private final IFolder m_scenarioFolder;

  public RestartSelectWizard( final IControlModel1D2D controlModel )
  {
    final IHandlerService handlerService = (IHandlerService)PlatformUI.getWorkbench().getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    final Shell shell = (Shell)context.getVariable( ISources.ACTIVE_SHELL_NAME );
    m_scenarioFolder = ScenarioHelper.getScenarioFolder();
    m_controlModel = controlModel;
    m_modelProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
    try
    {
      // Sometimes there is a NPE here... maybe wait until the models are loaded?
      m_resultModel = m_modelProvider.getModel( IScenarioResultMeta.class.getName() );
    }
    catch( final CoreException e )
    {
      Kalypso1d2dProjectPlugin.getDefault().getLog().log( e.getStatus() );
      ErrorDialog.openError( shell, Messages.getString( "org.kalypso.kalypso1d2d.pjt.wizards.RestartSelectWizard.5" ), Messages.getString( "org.kalypso.kalypso1d2d.pjt.wizards.RestartSelectWizard.6" ), e.getStatus() ); //$NON-NLS-1$ //$NON-NLS-2$
    }
  }

  @Override
  public void addPages( )
  {
    setWindowTitle( Messages.getString( "org.kalypso.kalypso1d2d.pjt.wizards.RestartSelectWizard.0" ) ); //$NON-NLS-1$

    final String title1 = Messages.getString( "org.kalypso.kalypso1d2d.pjt.wizards.RestartSelectWizard.8" ); //$NON-NLS-1$ 
    m_restartSelectWizardPage1 = new RestartSelectWizardPage1( "restartSelectionPage1", title1, m_resultModel ); //$NON-NLS-1$

    m_restartSelectWizardPage1.addAction( new ImportRestartAction( m_restartSelectWizardPage1, m_scenarioFolder, m_modelProvider, m_resultModel ) );

    m_restartSelectWizardPage1.setFilter( new DocumentResultViewerFilter() );
    m_restartSelectWizardPage1.setComparator( new Result1d2dMetaComparator() );

    addPage( m_restartSelectWizardPage1 );

    final String title2 = Messages.getString( "org.kalypso.kalypso1d2d.pjt.wizards.RestartSelectWizard.10" ); //$NON-NLS-1$
    m_restartSelectWizardPage2 = new RestartSelectWizardPage2( "restartSelectionPage2", title2, null ); //$NON-NLS-1$ 
    addPage( m_restartSelectWizardPage2 );

    configureRestartSelectPage();
  }

  private void configureRestartSelectPage( )
  {
    m_restartSelectWizardPage1.setTitle( Messages.getString( "org.kalypso.kalypso1d2d.pjt.wizards.RestartSelectWizard.3" ) ); //$NON-NLS-1$
    m_restartSelectWizardPage1.setDescription( Messages.getString( "org.kalypso.kalypso1d2d.pjt.wizards.RestartSelectWizard.4" ) ); //$NON-NLS-1$

    final List<IResultMeta> checkedElements = new ArrayList<>();

    final List<IRestartInfo> restartInfos = m_controlModel.getRestartInfos();
    for( final IRestartInfo restartInfo : restartInfos )
    {
      final String restartUnitID = restartInfo.getCalculationUnitID();
      final ICalcUnitResultMeta calcUnitMetaResult = m_resultModel.findCalcUnitMetaResult( restartUnitID );
      if( calcUnitMetaResult == null )
        continue;

      final String stepResultFilePath = restartInfo.getRestartFilePath().toString();
      final IFeatureBindingCollection<IResultMeta> calcUnitChildren = calcUnitMetaResult.getChildren();
      for( final IResultMeta calcUnitChild : calcUnitChildren )
      {
        if( calcUnitChild instanceof IStepResultMeta )
        {
          final IStepResultMeta stepResult = (IStepResultMeta)calcUnitChild;
          final IFeatureBindingCollection<IResultMeta> children = stepResult.getChildren();
          for( final IResultMeta resultMeta : children )
          {
            if( resultMeta instanceof IDocumentResultMeta )
            {
              final IDocumentResultMeta docResult = (IDocumentResultMeta)resultMeta;

              if( docResult.getDocumentType().equals( IDocumentResultMeta.DOCUMENTTYPE.nodes ) )
              {
                final String docPath = docResult.getFullPath().toString();
                if( docPath.equals( stepResultFilePath ) )
                  checkedElements.add( calcUnitChild );
              }
            }
          }
        }
      }
    }

    m_restartSelectWizardPage1.setInitialCheckedElements( checkedElements.toArray() );
  }

  @Override
  public boolean performFinish( )
  {
    final List<IRestartInfo> restartInfos = m_controlModel.getRestartInfos();
    restartInfos.clear();
    final IResultMeta[] selectedResults;
    if( getContainer().getCurrentPage().equals( m_restartSelectWizardPage2 ) )
      selectedResults = m_restartSelectWizardPage2.getSortedResults();
    else
      selectedResults = m_restartSelectWizardPage1.getSelectedResults();
    for( final IResultMeta element : selectedResults )
    {
      if( element instanceof StepResultMeta )
      {
        final IStepResultMeta result = (IStepResultMeta)element;
        result.setRestart( true );
        final IRestartInfo restartInfo = m_controlModel.addRestartInfo();
        restartInfo.setCalculationUnitID( ((ICalcUnitResultMeta)result.getOwner()).getCalcUnit() );
        restartInfo.setStepResultMetaID( result.getId() );

        // TODO: implement accessing zip file!
        final IFeatureBindingCollection<IResultMeta> children = result.getChildren();

        for( final IResultMeta resultMeta : children )
        {
          if( resultMeta instanceof IDocumentResultMeta )
          {
            final IDocumentResultMeta docResult = (IDocumentResultMeta)resultMeta;
            if( docResult.getDocumentType() == IDocumentResultMeta.DOCUMENTTYPE.nodes )
              restartInfo.setRestartFilePath( docResult.getFullPath().toPortableString() );
          }
        }
      }
    }
    try
    {
      // TODO: check if really something has changed, else we always get a dirty control model, which is not nice for
      // the user

      /* post empty command in order to make pool dirty. */
      ((ICommandPoster)m_modelProvider).postCommand( IControlModelGroup.class.getName(), new EmptyCommand( "You are dirty now, pool!", false ) ); //$NON-NLS-1$
    }
    catch( final Exception e )
    {
      // will never happen?
      e.printStackTrace();
    }
    return true;
  }
}
