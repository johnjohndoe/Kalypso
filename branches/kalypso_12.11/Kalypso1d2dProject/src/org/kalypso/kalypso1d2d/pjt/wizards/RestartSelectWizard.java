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
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.IRestartInfo;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Dejan Antanaskovic
 */
public class RestartSelectWizard extends Wizard
{
  private IScenarioResultMeta m_resultModel;

  private final IScenarioDataProvider m_modelProvider;

  private final IControlModel1D2D m_controlModel;

  private final IFolder m_scenarioFolder;

  private final RestartSelectData m_data;

  public RestartSelectWizard( final IControlModel1D2D controlModel )
  {
    setDialogSettings( DialogSettingsUtils.getDialogSettings( Kalypso1d2dProjectPlugin.getDefault(), getClass().getName() ) );

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
      ErrorDialog.openError( shell, getWindowTitle(), Messages.getString( "org.kalypso.kalypso1d2d.pjt.wizards.RestartSelectWizard.6" ), e.getStatus() ); //$NON-NLS-1$ //$NON-NLS-2$
    }

    final IResultMeta[] checkedResults = getCheckedResults();

    m_data = new RestartSelectData( m_scenarioFolder, m_resultModel, checkedResults );
  }

  @Override
  public void addPages( )
  {
    final RestartSelectWizardPage1 page = new RestartSelectWizardPage1( "restartSelectionPage1", m_data ); //$NON-NLS-1$
    page.addAction( new ImportRestartAction( page, m_scenarioFolder, m_modelProvider, m_resultModel ) );

    addPage( page );
  }

  private IStepResultMeta[] getCheckedResults( )
  {
    final List<IStepResultMeta> checkedElements = new ArrayList<>();

    final List<IRestartInfo> restartInfos = m_controlModel.getRestartInfos();
    for( final IRestartInfo restartInfo : restartInfos )
    {
      final String restartUnitID = restartInfo.getCalculationUnitID();
      final ICalcUnitResultMeta calcUnitMetaResult = m_resultModel.findCalcUnitMetaResult( restartUnitID );
      if( calcUnitMetaResult == null )
        continue;

      final String stepResultFilePath = restartInfo.getRestartFilePath().toString();

      // FIXME: handle results outside current scenario...

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
                  checkedElements.add( stepResult );
              }
            }
          }
        }
      }
    }

    return checkedElements.toArray( new IStepResultMeta[checkedElements.size()] );
  }

  @Override
  public boolean performFinish( )
  {
    final List<IRestartInfo> restartInfos = m_controlModel.getRestartInfos();
    restartInfos.clear();

    final IStepResultMeta[] restartResults = m_data.getRestartResults();
    for( final IResultMeta element : restartResults )
    {
      if( element instanceof IStepResultMeta )
      {
        final IStepResultMeta result = (IStepResultMeta)element;

        final IRestartInfo restartInfo = m_controlModel.addRestartInfo();
        restartInfo.setCalculationUnitID( ((ICalcUnitResultMeta)result.getOwner()).getCalcUnit() );
        restartInfo.setStepResultMetaID( result.getId() );

        // TODO: implement accessing zip file! FIXME: why?

        /* find node result for this step */
        final IFeatureBindingCollection<IResultMeta> children = result.getChildren();
        for( final IResultMeta resultMeta : children )
        {
          if( resultMeta instanceof IDocumentResultMeta )
          {
            final IDocumentResultMeta docResult = (IDocumentResultMeta)resultMeta;
            if( docResult.getDocumentType() == IDocumentResultMeta.DOCUMENTTYPE.nodes )
            {
              final String restartPath = ResultMeta1d2dHelper.buildFullLocation( docResult, m_scenarioFolder );
              restartInfo.setRestartFilePath( restartPath );
            }
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