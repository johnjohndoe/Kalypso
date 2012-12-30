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
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypso1d2d.pjt.map.MapUtils;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ui.wizards.results.filters.NonMapDataResultViewerFilter;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * Wizard to add result themes to the map.
 * 
 * @author Thomas Jung
 */
public class AddResultThemeWizard extends Wizard implements IWorkbenchWizard
{
  private final static String PAGE_SELECT_RESULTS_NAME = "selectResults"; //$NON-NLS-1$

  private IKalypsoLayerModell m_modell;

  private IScenarioResultMeta m_resultModel;

  private ICommandTarget m_commandTarget;

  private ThemeConstructionFactory m_themeConstructionFactory;

  public AddResultThemeWizard( )
  {
    setWindowTitle( Messages.getString( "org.kalypso.ui.wizards.results.AddResultThemeWizard.1" ) ); //$NON-NLS-1$
  }

  @Override
  public void addPages( )
  {
    final String title = Messages.getString( "org.kalypso.ui.wizards.results.AddResultThemeWizard.2" ); //$NON-NLS-1$

    final SelectResultData data = new SelectResultData( m_resultModel );
    data.setShowOptions( true );

    final SelectResultWizardPage selectResultWizardPage = new SelectResultWizardPage( PAGE_SELECT_RESULTS_NAME, title, data );

    selectResultWizardPage.setFactory( m_themeConstructionFactory );
    selectResultWizardPage.setFilter( new NonMapDataResultViewerFilter() );

    addPage( selectResultWizardPage );

    // Page: Neues Thema konfigurieren???
  }

  public void setCommandTarget( final ICommandTarget commandTarget )
  {
    m_commandTarget = commandTarget;
  }

  public void setMapModel( final IKalypsoLayerModell modell )
  {
    m_modell = modell;
  }

  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    final IHandlerService handlerService = (IHandlerService)workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    final Shell shell = (Shell)context.getVariable( ISources.ACTIVE_SHELL_NAME );
    final IScenarioDataProvider modelProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
    try
    {
      final IFolder scenarioFolder = KalypsoAFGUIFrameworkPlugin.getActiveWorkContext().getCurrentCase().getFolder();
      m_themeConstructionFactory = new ThemeConstructionFactory( scenarioFolder );

      // Sometimes there is a NPE here... maybe wait until the models are loaded?
      m_resultModel = modelProvider.getModel( IScenarioResultMeta.class.getName() );
    }
    catch( final CoreException e )
    {
      Kalypso1d2dProjectPlugin.getDefault().getLog().log( e.getStatus() );
      ErrorDialog.openError( shell, Messages.getString( "org.kalypso.ui.wizards.results.AddResultThemeWizard.3" ), Messages.getString( "org.kalypso.ui.wizards.results.AddResultThemeWizard.4" ), e.getStatus() ); //$NON-NLS-1$ //$NON-NLS-2$
    }
  }

  @Override
  public boolean performFinish( )
  {
    final SelectResultWizardPage page = (SelectResultWizardPage)getPage( PAGE_SELECT_RESULTS_NAME );
    final IResultMeta[] results = page.getSelectedResults();
    final IKalypsoLayerModell modell = m_modell;

    if( modell == null )
    {
      System.out.println( "No map template available." ); //$NON-NLS-1$
      return false;
    }

    final ICoreRunnableWithProgress operation = new ICoreRunnableWithProgress()
    {
      @Override
      @SuppressWarnings( "synthetic-access" )
      public IStatus execute( final IProgressMonitor monitor )
      {
        return MapUtils.addThemes( modell, m_commandTarget, results, m_themeConstructionFactory, monitor );
      }
    };

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, true, operation );
    Kalypso1d2dProjectPlugin.getDefault().getLog().log( status );
    ErrorDialog.openError( getShell(), Messages.getString( "org.kalypso.ui.wizards.results.AddResultThemeWizard.5" ), Messages.getString( "org.kalypso.ui.wizards.results.AddResultThemeWizard.6" ), status ); //$NON-NLS-1$ //$NON-NLS-2$

    return status.isOK();
  }
}