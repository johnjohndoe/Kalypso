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
package org.kalypso.ui.wizards.differences;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.afgui.model.ICommandPoster;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.differences.MathOperator;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ui.wizards.results.SelectResultData;
import org.kalypso.ui.wizards.results.SelectResultWizardPage;
import org.kalypso.ui.wizards.results.filters.DocumentResultViewerFilter;
import org.kalypso.ui.wizards.results.filters.NonTinDocumentResultViewerFilter;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * Wizard to show length sections to the chart view.
 * 
 * @author Thomas Jung
 */
public class GenerateDifferenceResultTinWizard extends Wizard
{
  private static final String PAGE_SELECT_DESTINATION_RESULTS_NAME = "selectDestinationResults"; //$NON-NLS-1$

  private static final String PAGE_SELECT_MASTER_RESULTS_NAME = "selectMasterResults"; //$NON-NLS-1$

  private static final String PAGE_SELECT_SLAVE_RESULTS_NAME = "selectSlaveResults"; //$NON-NLS-1$

  private final IScenarioResultMeta m_resultModel;

  private IFile m_selectedResultFile;

  private final IFolder m_scenarioFolder;

  private final IScenarioDataProvider m_modelProvider;

  private final DifferenceResultData m_destinationData;

  public GenerateDifferenceResultTinWizard( final IFolder scenarioFolder, final IScenarioResultMeta resultModel, final IScenarioDataProvider modelProvider )
  {
    m_scenarioFolder = scenarioFolder;
    m_resultModel = resultModel;
    m_modelProvider = modelProvider;

    m_destinationData = new DifferenceResultData( m_resultModel );

    setWindowTitle( Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.3" ) ); //$NON-NLS-1$

    setNeedsProgressMonitor( true );
    setDialogSettings( DialogSettingsUtils.getDialogSettings( Kalypso1d2dProjectPlugin.getDefault(), getClass().getName() ) );
  }

  @Override
  public void addPages( )
  {
    final NonTinDocumentResultViewerFilter resultFilter = new NonTinDocumentResultViewerFilter();

    /* master */
    final SelectResultData masterData = new SelectResultData( m_resultModel );
    masterData.setShowOptions( true );

    final String titleMaster = Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.4" ); //$NON-NLS-1$
    final SelectResultWizardPage masterPage = new SelectResultWizardPage( PAGE_SELECT_MASTER_RESULTS_NAME, titleMaster, masterData );

    masterPage.setFilter( resultFilter );

    /* slave */
    final SelectResultData slaveData = new SelectResultData( m_resultModel );
    slaveData.setShowOptions( true );

    final String titleSlave = Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.5" ); //$NON-NLS-1$
    final SelectResultWizardPage slavePage = new SelectResultWizardPage( PAGE_SELECT_SLAVE_RESULTS_NAME, titleSlave, slaveData );

    slavePage.setFilter( resultFilter );

    /* destination */
    final String titleDestination = Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.6" ); //$NON-NLS-1$
    final SelectResultWizardPage destinationPage = new SelectResultWizardPage( PAGE_SELECT_DESTINATION_RESULTS_NAME, titleDestination, m_destinationData );

    destinationPage.setFactory( new GenerateDifferenceControlFactory( m_destinationData ) );
    destinationPage.setFilter( new DocumentResultViewerFilter() );

    addPage( masterPage );
    addPage( slavePage );
    addPage( destinationPage );
  }

  @Override
  public boolean performFinish( )
  {
    try
    {
      final TinDifferenceData data = configureData();

      /* Start */
      final ICoreRunnableWithProgress op = new GenerateDifferenceResultTinOperation( data );

      final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, op );

      if( status.isOK() )
        saveResultModel();
      else
      {
        Kalypso1d2dProjectPlugin.getDefault().getLog().log( status );
        StatusDialog.open( getShell(), status, getWindowTitle() );
      }

      return !status.matches( IStatus.ERROR );
    }
    catch( final CoreException e )
    {
      /* happens for validation problem when creating data -> never close wizard */
      StatusDialog.open( getShell(), e.getStatus(), getWindowTitle() );
      return false;
    }
  }

  private void saveResultModel( )
  {
    try
    {
      ((ICommandPoster)m_modelProvider).postCommand( IScenarioResultMeta.class.getName(), new EmptyCommand( "", false ) ); //$NON-NLS-1$
      m_modelProvider.saveModel( IScenarioResultMeta.class.getName(), new NullProgressMonitor() );
    }
    catch( final Exception e )
    {
      // TODO: better error handling!
      e.printStackTrace();
    }
  }

  private TinDifferenceData configureData( ) throws CoreException
  {
    final MathOperator operator = MathOperator.eMinus;

    /* check user input */
    final IDocumentResultMeta masterResult = getMasterResult();
    final IDocumentResultMeta slaveResult = getSlaveResult();

    if( !slaveResult.getDocumentType().equals( masterResult.getDocumentType() ) )
    {
      final String message = Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.11" ); //$NON-NLS-1$  
      final IStatus status = new Status( IStatus.WARNING, Kalypso1d2dProjectPlugin.PLUGIN_ID, message );
      throw new CoreException( status );

      // REMARK: it was previously possible to proceed here, but that makes no sense.
      // if( !MessageDialog.openQuestion( getShell(), Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.11" ), Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.12" ) ) ) //$NON-NLS-1$ //$NON-NLS-2$
      // return false;
    }

    final IStepResultMeta destinationResult = getDestinationResult();
    final String destinationName = m_destinationData.getDestinationName();

    return new TinDifferenceData( masterResult, slaveResult, destinationResult, destinationName, operator, m_scenarioFolder );
  }

  // FIXME: not nice to validate after user has entered the values; instead the wizard should directly validate the input
  private IDocumentResultMeta getMasterResult( ) throws CoreException
  {
    final SelectResultWizardPage masterResultPage = (SelectResultWizardPage)getPage( PAGE_SELECT_MASTER_RESULTS_NAME );
    final IResultMeta[] masterResults = masterResultPage.getSelectedResults();

    if( masterResults.length != 1 || !(masterResults[0] instanceof IDocumentResultMeta) )
    {
      final String message = Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.7" ) + '\n' + Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.8" ); //$NON-NLS-1$ //$NON-NLS-2$ 
      final IStatus status = new Status( IStatus.WARNING, Kalypso1d2dProjectPlugin.PLUGIN_ID, message );
      throw new CoreException( status );
    }

    return (IDocumentResultMeta)masterResults[0];
  }

  private IDocumentResultMeta getSlaveResult( ) throws CoreException
  {
    final SelectResultWizardPage slaveResultPage = (SelectResultWizardPage)getPage( PAGE_SELECT_SLAVE_RESULTS_NAME );
    final IResultMeta[] slaveResults = slaveResultPage.getSelectedResults();

    if( slaveResults.length != 1 || !(slaveResults[0] instanceof IDocumentResultMeta) )
    {
      final String message = Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.9" ) + '\n' + Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.10" ); //$NON-NLS-1$ //$NON-NLS-2$
      final IStatus status = new Status( IStatus.WARNING, Kalypso1d2dProjectPlugin.PLUGIN_ID, message );
      throw new CoreException( status );
    }

    return (IDocumentResultMeta)slaveResults[0];
  }

  private IStepResultMeta getDestinationResult( ) throws CoreException
  {
    final SelectResultWizardPage destinationResultPage = (SelectResultWizardPage)getPage( PAGE_SELECT_DESTINATION_RESULTS_NAME );
    final IResultMeta[] destinationResults = destinationResultPage.getSelectedResults();

    if( destinationResults.length != 1 )
    {
      final String message = Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.13" ) + '\n' + Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.14" ); //$NON-NLS-1$ //$NON-NLS-2$
      final IStatus status = new Status( IStatus.WARNING, Kalypso1d2dProjectPlugin.PLUGIN_ID, message );
      throw new CoreException( status );
    }

    final IResultMeta destResult = destinationResults[0];

    if( destResult instanceof IStepResultMeta )
      return (IStepResultMeta)destResult;

    final String message = Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.15" ) + '\n' + Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.16" ); //$NON-NLS-1$ //$NON-NLS-2$
    final IStatus status = new Status( IStatus.WARNING, Kalypso1d2dProjectPlugin.PLUGIN_ID, message );
    throw new CoreException( status );
  }

  public IFile getSelection( )
  {
    return m_selectedResultFile;
  }
}