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
package org.kalypso.ui.wizards.differences;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.afgui.model.ICommandPoster;
import org.kalypso.commons.command.EmptyCommand;
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
import org.kalypso.ui.wizards.results.Result1d2dMetaComparator;
import org.kalypso.ui.wizards.results.SelectResultWizardPage;
import org.kalypso.ui.wizards.results.ThemeConstructionFactory;
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

  public GenerateDifferenceResultTinWizard( final IFolder scenarioFolder, final IScenarioResultMeta resultModel, final IScenarioDataProvider modelProvider )
  {
    m_scenarioFolder = scenarioFolder;
    m_resultModel = resultModel;
    m_modelProvider = modelProvider;
    setWindowTitle( Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.3" ) ); //$NON-NLS-1$

    setNeedsProgressMonitor( true );
  }

  @Override
  public void addPages( )
  {
    // select master document page
    final NonTinDocumentResultViewerFilter resultFilter = new NonTinDocumentResultViewerFilter();
    final Result1d2dMetaComparator comparator = new Result1d2dMetaComparator();

    final String titleMaster = Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.4" ); //$NON-NLS-1$
    final SelectResultWizardPage selectMasterResultWizardPage = new SelectResultWizardPage( PAGE_SELECT_MASTER_RESULTS_NAME, titleMaster );
    selectMasterResultWizardPage.setFilter( resultFilter );
    selectMasterResultWizardPage.setComparator( comparator );

    final String titleSlave = Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.5" ); //$NON-NLS-1$
    final SelectResultWizardPage selectSlaveResultWizardPage = new SelectResultWizardPage( PAGE_SELECT_SLAVE_RESULTS_NAME, titleSlave );
    selectSlaveResultWizardPage.setFilter( resultFilter );
    selectSlaveResultWizardPage.setComparator( comparator );

    final ThemeConstructionFactory themeConstructionFactory = new ThemeConstructionFactory( m_scenarioFolder );
    final String titleDestination = Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.6" ); //$NON-NLS-1$
    final SelectResultWizardPage selectDestinationResultWizardPage = new SelectResultWizardPage( PAGE_SELECT_DESTINATION_RESULTS_NAME, titleDestination );
    selectDestinationResultWizardPage.setFactory( themeConstructionFactory );
    selectDestinationResultWizardPage.setFilter( new DocumentResultViewerFilter() );
    selectDestinationResultWizardPage.setComparator( comparator );

    selectMasterResultWizardPage.setResultMeta( m_resultModel );
    selectSlaveResultWizardPage.setResultMeta( m_resultModel );
    selectDestinationResultWizardPage.setResultMeta( m_resultModel );

    addPage( selectMasterResultWizardPage );
    addPage( selectSlaveResultWizardPage );
    addPage( selectDestinationResultWizardPage );
  }

  @Override
  public boolean performFinish( )
  {
    final MathOperator operator = MathOperator.eMinus;

    IDocumentResultMeta.DOCUMENTTYPE masterDocType = null;
    IDocumentResultMeta.DOCUMENTTYPE slaveDocType = null;

    /* check user input */
    // master
    final SelectResultWizardPage masterResultPage = (SelectResultWizardPage)getPage( PAGE_SELECT_MASTER_RESULTS_NAME );
    final IResultMeta[] masterResults = masterResultPage.getSelectedResults();

    if( masterResults.length == 0 || !(masterResults[0] instanceof IDocumentResultMeta) )
    {
      MessageDialog.openInformation( getShell(), Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.7" ), Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.8" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      return false;
    }
    else
    {
      if( masterResults[0] instanceof IDocumentResultMeta )
      {
        masterDocType = ((IDocumentResultMeta)masterResults[0]).getDocumentType();
      }
    }

    // slave
    final SelectResultWizardPage slaveResultPage = (SelectResultWizardPage)getPage( PAGE_SELECT_SLAVE_RESULTS_NAME );
    final IResultMeta[] slaveResults = slaveResultPage.getSelectedResults();

    if( slaveResults.length == 0 || !(slaveResults[0] instanceof IDocumentResultMeta) )
    {
      MessageDialog.openInformation( getShell(), Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.9" ), Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.10" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      return false;
    }
    else
    {
      if( slaveResults[0] instanceof IDocumentResultMeta )
      {
        slaveDocType = ((IDocumentResultMeta)slaveResults[0]).getDocumentType();
      }
    }

    if( !slaveDocType.equals( masterDocType ) )
    {
      if( !MessageDialog.openQuestion( getShell(), Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.11" ), Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.12" ) ) ) //$NON-NLS-1$ //$NON-NLS-2$
        return false;
    }

    final SelectResultWizardPage destinationResultPage = (SelectResultWizardPage)getPage( PAGE_SELECT_DESTINATION_RESULTS_NAME );
    final IResultMeta[] destinationResults = destinationResultPage.getSelectedResults();

    // destination
    if( destinationResults.length == 0 )
    {
      MessageDialog.openInformation( getShell(), Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.13" ), Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.14" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      return false;
    }
    else
    {
      IResultMeta destResult = null;

      // TODO: allow the user to set an individual result name and store information about master and slave in the ResultMeta entry

      // take the first selected step result
      for( final IResultMeta resultMeta : destinationResults )
      {
        if( resultMeta instanceof IStepResultMeta )
        {
          destResult = resultMeta;
        }
      }

      if( destResult == null )
      {
        MessageDialog.openInformation( getShell(), Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.15" ), Messages.getString( "org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.16" ) ); //$NON-NLS-1$ //$NON-NLS-2$
        return false;
      }
    }

    /* Start */
    final ICoreRunnableWithProgress op = new GenerateDifferenceResultTinOperation( operator, masterResults, destinationResults, slaveResults, m_scenarioFolder );

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, op );
    if( !status.isOK() )
    {
      //ErrorDialog.openError( getShell(), getWindowTitle(), Messages.getString("org.kalypso.ui.wizards.differences.GenerateDifferenceResultTinWizard.37"), status ); //$NON-NLS-1$
      Kalypso1d2dProjectPlugin.getDefault().getLog().log( status );
      StatusDialog.open( getShell(), status, getWindowTitle() );
    }
    else
    {
      try
      {
        ((ICommandPoster)m_modelProvider).postCommand( IScenarioResultMeta.class.getName(), new EmptyCommand( "", false ) ); //$NON-NLS-1$
        m_modelProvider.saveModel( IScenarioResultMeta.class.getName(), new NullProgressMonitor() );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }

    return !status.matches( IStatus.ERROR );
  }

  public IFile getSelection( )
  {
    return m_selectedResultFile;
  }
}