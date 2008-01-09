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

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IPageChangeProvider;
import org.eclipse.jface.dialogs.IPageChangedListener;
import org.eclipse.jface.dialogs.PageChangedEvent;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Button;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;

import de.renew.workflow.connector.cases.ICaseDataProvider;

/**
 * A wizard showing the progress of a rma10s calculation.
 * 
 * @author Gernot Belger
 */
public class RMA10CalculationWizard extends Wizard implements IWizard
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

  public RMA10CalculationWizard( final RMA10Calculation calculation, final ResultManager resultManager, final IContainer unitFolder, final ICaseDataProvider<IModel> caseDataProvider, final IGeoLog geoLog )
  {
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

  private boolean runCalculation( )
  {
    m_calcPage.runCalculation();

    final IStatus simulationStatus = m_calcPage.getSimulationStatus();

    /* Jump to next page and set simulation status to result page */
    addPage( m_resultPage );
    getContainer().updateButtons();

    /* If result processing starts immediately, show that page */
    if( m_calcPage.getStartResultProcessing() )
      getContainer().showPage( m_resultPage );

    /* If canceled, cancel the whole dialog at once. */
    return simulationStatus.matches( IStatus.CANCEL );
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
    final boolean canFinish = super.canFinish();

    if( !canFinish )
      return false;

    final IWizardPage currentPage = getContainer().getCurrentPage();
    /* Do not let run result processing on calc page */
    if( currentPage == m_calcPage && m_calcPage.getSimulationStatus() != null )
      return false;

    return canFinish;
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    if( m_calcPage.getSimulationStatus() == null )
    {
      final boolean closeWizard = runCalculation();

      getContainer().updateButtons();

      /* If result processing starts immediately, fall through to result processing */
      if( !m_calcPage.getStartResultProcessing() )
        return closeWizard;
    }

    if( m_resultPage.getResultStatus() == null )
    {
      final boolean closeWizard = runResultProcessing();

      getContainer().updateButtons();

      // HACK: disable cancel, after result processing, as cancelling does not change anythin now
      final IWizardContainer container = getContainer();
      if( container instanceof WizardDialog2 )
      {
        final Button button = ((WizardDialog2) container).getButton( IDialogConstants.CANCEL_ID );
        button.setEnabled( false );
      }

      // TODO: if close wizard after result processing: fall through
      return closeWizard;
    }

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

    return super.performCancel();
  }

}
