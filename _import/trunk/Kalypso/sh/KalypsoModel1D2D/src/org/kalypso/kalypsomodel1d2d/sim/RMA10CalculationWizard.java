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
package org.kalypso.kalypsomodel1d2d.sim;

import java.io.File;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IPageChangeProvider;
import org.eclipse.jface.dialogs.IPageChangedListener;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.PageChangedEvent;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Button;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.binding.commons.IStatusCollection;

import de.renew.workflow.connector.cases.ICaseDataProvider;

/**
 * A wizard showing the progress of a rma10s calculation.
 * 
 * @author Gernot Belger
 */
public class RMA10CalculationWizard extends Wizard implements IWizard, ISimulation1D2DConstants
{
  private final IPageChangedListener m_pageChangeListener = new IPageChangedListener()
  {
    public void pageChanged( PageChangedEvent event )
    {
      handlePageChanged( event );
    }
  };

  private final RMA10CalculationPage m_calcPage;

  private final RMA10ResultPage m_resultPage;

  private final IGeoLog m_geoLog;

  private final IContainer m_unitFolder;

  public RMA10CalculationWizard( final RMA10Calculation calculation, final ResultManager resultManager, final IContainer unitFolder, final ICaseDataProvider<IModel> caseDataProvider, final IGeoLog geoLog )
  {
    m_unitFolder = unitFolder;
    m_geoLog = geoLog;
    m_calcPage = new RMA10CalculationPage( "calcPage", calculation );
    m_resultPage = new RMA10ResultPage( "resultPage", resultManager, unitFolder, caseDataProvider );

    setNeedsProgressMonitor( true );
    setForcePreviousAndNextButtons( true );
  }

  protected void handlePageChanged( final PageChangedEvent event )
  {
    if( event.getSelectedPage() == m_calcPage )
    {
      if( m_calcPage.getSimulationStatus() == null )
        setFinishText( "Start" );
    }
  }

  private void setFinishText( final String buttonText )
  {
    final IWizardContainer container = getContainer();
    if( container instanceof WizardDialog2 )
    {
      final Button button = ((WizardDialog2) container).getButton( IDialogConstants.FINISH_ID );
      button.setText( buttonText );
    }
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#setContainer(org.eclipse.jface.wizard.IWizardContainer)
   */
  @Override
  public void setContainer( final IWizardContainer wizardContainer )
  {
    final IWizardContainer oldContainer = getContainer();
    if( oldContainer instanceof IPageChangeProvider )
      ((IPageChangeProvider) oldContainer).removePageChangedListener( m_pageChangeListener );

    super.setContainer( wizardContainer );

    if( wizardContainer instanceof IPageChangeProvider )
      ((IPageChangeProvider) wizardContainer).addPageChangedListener( m_pageChangeListener );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#dispose()
   */
  @Override
  public void dispose( )
  {
    final IWizardContainer oldContainer = getContainer();
    if( oldContainer instanceof IPageChangeProvider )
      ((IPageChangeProvider) oldContainer).removePageChangedListener( m_pageChangeListener );

    super.dispose();
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    addPage( m_calcPage );
  }

  private IStatus runCalculation( )
  {
    m_calcPage.runCalculation();

    final IStatus simulationStatus = m_calcPage.getSimulationStatus();

    /* Jump to next page and set simulation status to result page */
    addPage( m_resultPage );
    getContainer().updateButtons();

    if( simulationStatus.matches( IStatus.ERROR ) )
    {
      // HACK: disable cancel, after result processing, as canceling will not change anything from now on
      final IWizardContainer container = getContainer();
      if( container instanceof WizardDialog2 )
      {
        final Button cancelButton = ((WizardDialog2) container).getButton( IDialogConstants.CANCEL_ID );
        cancelButton.setEnabled( false );
        final Button nextButton = ((WizardDialog2) container).getButton( IDialogConstants.NEXT_ID );
        nextButton.setEnabled( false );
      }

      setFinishText( IDialogConstants.FINISH_LABEL );

      return simulationStatus;
    }

    /* If result processing starts immediately, show that page */
    if( m_calcPage.getStartResultProcessing() )
      getContainer().showPage( m_resultPage );

    return simulationStatus;
  }

  private boolean runResultProcessing( )
  {
    m_resultPage.runResultProcessing();

    setFinishText( IDialogConstants.FINISH_LABEL );

    /* Jump to next page and set simulation status to result page */
    final IStatus resultStatus = m_resultPage.getResultStatus();

    /* If canceled, cancel the whole dialog at once. */
    return resultStatus.matches( IStatus.CANCEL );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#canFinish()
   */
  @Override
  public boolean canFinish( )
  {
    final IStatus simulationStatus = m_calcPage.getSimulationStatus();

    if( simulationStatus != null && simulationStatus.matches( IStatus.ERROR ) )
      return true;

    final boolean canFinish = super.canFinish();

    if( !canFinish )
      return false;

    final IWizardPage currentPage = getContainer().getCurrentPage();
    /* Do not let run result processing on calc page */
    if( currentPage == m_calcPage && simulationStatus != null )
      return false;

    return canFinish;
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    final IWizardContainer container = getContainer();

    if( m_calcPage.getSimulationStatus() == null )
    {
      final IStatus simulationStatus = runCalculation();

      /* On cancel, directly close the dialog */
      if( simulationStatus.matches( IStatus.CANCEL ) )
      {
        saveLog();
        return true;
      }

      /* On error, don't close, show the error message */
      if( simulationStatus.matches( IStatus.ERROR ) )
        return false;

      /* We could start result processing: immediately do it, if user wishes to */
      if( !m_calcPage.getStartResultProcessing() )
        return false;

      // fall through to result processing
    }

    /* On finish after simulation error, close the dialog */
    if( m_calcPage.getSimulationStatus().matches( IStatus.ERROR ) )
    {
      saveLog();
      return true;
    }

    if( m_resultPage.getResultStatus() == null )
    {
      final boolean closeWizard = runResultProcessing();
      if( closeWizard )
      {
        saveLog();
        return true;
      }

      container.updateButtons();

      // HACK: disable cancel, after result processing, as canceling will not change anything from now on
      if( container instanceof WizardDialog2 )
      {
        final Button button = ((WizardDialog2) container).getButton( IDialogConstants.CANCEL_ID );
        button.setEnabled( false );
      }

      return false;
    }

    saveLog();

    return true;
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performCancel()
   */
  @Override
  public boolean performCancel( )
  {
    if( m_calcPage.getSimulationStatus() != null )
    {
      /* If calculation was made, but user canceled this dialog before result processing, put a message in the log. */
      m_geoLog.log( IStatus.WARNING, ISimulation1D2DConstants.CODE_POST, "Ergebnisauswertung durch Benutzer abgebrochen.", null, null );
    }

    /* Only save log on cancel if something happened, else, everything stays as before. */
    // TODO: still a problem: if the user only simulates, the log shows the simulation, but results are still like
    // before.
    if( m_calcPage.getSimulationStatus() != null )
      saveLog();

    return true;
  }

  private void saveLog( )
  {
    try
    {
      /* Close and save the geo log */
      m_geoLog.close();
      final IStatusCollection statusCollection = m_geoLog.getStatusCollection();
      final GMLWorkspace workspace = statusCollection.getWrappedFeature().getWorkspace();
      // REMARK: we directly save the log into the unit-folder, as the results already where moved from the output
      // directory
      // REMARK2: the calc unit meta may be not set, but the simulation log is written anyway... Probably we should
      // change this?
      if( !m_unitFolder.exists() && m_unitFolder instanceof IFolder )
        ((IFolder) m_unitFolder).create( true, true, new NullProgressMonitor() );

      final File simDir = m_unitFolder.getLocation().toFile();
      final File loggerFile = new File( simDir, SIMULATION_LOG_GML );
      GmlSerializer.serializeWorkspace( loggerFile, workspace, "UTF-8" );
      m_unitFolder.refreshLocal( IFolder.DEPTH_ONE, new NullProgressMonitor() );
    }
    catch( final Throwable e )
    {
      MessageDialog.openError( getShell(), getWindowTitle(), "Simulation-Log konnte nicht gespeichert werden: " + e.toString() );
    }
  }

}
