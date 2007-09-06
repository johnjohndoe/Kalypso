/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import java.io.File;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ui.wizards.results.ResultViewerFilter;
import org.kalypso.ui.wizards.results.SelectResultWizardPage;
import org.kalypso.ui.wizards.results.ThemeConstructionFactory;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

import de.renew.workflow.connector.cases.CaseHandlingSourceProvider;
import de.renew.workflow.connector.cases.ICaseDataProvider;
import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * @author Dejan Antanaskovic
 * 
 */
public class RestartSelectWizard extends Wizard implements INewWizard
{
  private SelectResultWizardPage m_restartSelectWizardPage;

  private final Feature m_feature;

  private IFolder m_scenarioFolder;

  private IScenarioResultMeta m_resultModel;

  public RestartSelectWizard( final Feature feature )
  {
    m_feature = feature;
    final IHandlerService handlerService = (IHandlerService) PlatformUI.getWorkbench().getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );
    final ICaseDataProvider<IFeatureWrapper2> modelProvider = (ICaseDataProvider<IFeatureWrapper2>) context.getVariable( CaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
    m_scenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
    try
    {
      // Sometimes there is a NPE here... maybe wait until the models are loaded?
      m_resultModel = modelProvider.getModel( IScenarioResultMeta.class );
    }
    catch( CoreException e )
    {
      Kalypso1d2dProjectPlugin.getDefault().getLog().log( e.getStatus() );
      ErrorDialog.openError( shell, "1D2D-Ergebnisse", "Ergebnis-Metadaten nicht vorhanden.", e.getStatus() );
    }
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    setWindowTitle( Messages.getString( "RestartSelectWizard.0" ) ); //$NON-NLS-1$
    final ResultViewerFilter resultFilter = new ResultViewerFilter();
    final ThemeConstructionFactory themeConstructionFactory = new ThemeConstructionFactory( m_scenarioFolder );
    m_restartSelectWizardPage = new SelectResultWizardPage( "restartSelectionPage", "Ergebniss(e) zur Karte hinzufügen", null, resultFilter, themeConstructionFactory );
    m_restartSelectWizardPage.setResultMeta( m_resultModel );
    m_restartSelectWizardPage.setTitle( Messages.getString( "RestartSelectWizard.3" ) ); //$NON-NLS-1$
    m_restartSelectWizardPage.setDescription( Messages.getString( "RestartSelectWizard.4" ) ); //$NON-NLS-1$
    addPage( m_restartSelectWizardPage );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    // final String selectedPath = m_restartSelectWizardPage.getSelectedPath();
    final IResultMeta[] selectedResults = m_restartSelectWizardPage.getSelectedResults();
    for( int i = 0; i < selectedResults.length; i++ )
      m_feature.setProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_RESTART_PATH, selectedResults[i].getFullPath() );
    m_feature.setProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_RESTART, selectedResults.length > 0 );
    return true;
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  @SuppressWarnings("unchecked")
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
  }

  public File[] getSelectedFiles( )
  {
    // TODO Auto-generated method stub
    return null;
  }

}
