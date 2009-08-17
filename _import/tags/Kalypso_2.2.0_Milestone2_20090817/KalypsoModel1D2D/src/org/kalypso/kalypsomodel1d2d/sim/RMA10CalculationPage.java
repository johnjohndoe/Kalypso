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

import org.apache.commons.vfs.FileObject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.util.swt.StatusComposite;

/**
 * @author Gernot Belger
 */
public class RMA10CalculationPage extends WizardPage implements IWizardPage
{
  private static final String SETTING_START_RESULT_PROCESSING = "startResultProcessing"; //$NON-NLS-1$

  // private final RMA10Calculation m_calculation;
  private final RMAKalypsoSimulationRunner m_calculation;

  private IStatus m_simulationStatus;

  private StatusComposite m_statusComp;

  protected boolean m_startResultProcessing = false;

  private Button m_startResultProcessingCheck;

  private Spinner m_resultInterval;

  protected RMA10CalculationPage( final String pageName, final IGeoLog geoLog, final SzenarioDataProvider caseDataProvider ) throws CoreException
  {
    super( pageName );

    m_calculation = new RMAKalypsoSimulationRunner( geoLog, caseDataProvider );
    final ICalculationUnit calculationUnit = m_calculation.getControlModel().getCalculationUnit();

    setTitle( String.format( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10CalculationPage.0" ), calculationUnit.getName() ) ); //$NON-NLS-1$

    setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10CalculationPage.2" ) ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    /* Load dialog settings */
    final IDialogSettings dialogSettings = getDialogSettings();
    if( dialogSettings != null )
      m_startResultProcessing = dialogSettings.getBoolean( SETTING_START_RESULT_PROCESSING );

    final Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout() );

    /* Status composite */
    final Group statusGroup = new Group( composite, SWT.NONE );
    statusGroup.setLayout( new GridLayout() );
    statusGroup.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    statusGroup.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10CalculationPage.3" ) ); //$NON-NLS-1$
    m_statusComp = new StatusComposite( statusGroup, StatusComposite.DETAILS );
    m_statusComp.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    m_statusComp.setStatus( StatusUtilities.createStatus( IStatus.INFO, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10CalculationPage.4" ), null ) ); //$NON-NLS-1$

    final Group tweakGroup = new Group( composite, SWT.NONE );
    final GridLayout tweakLayout = new GridLayout();
    tweakLayout.numColumns = 2;
    tweakGroup.setLayout( tweakLayout );
    tweakGroup.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    tweakGroup.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10CalculationPage.5" ) ); //$NON-NLS-1$

    m_startResultProcessingCheck = new Button( tweakGroup, SWT.CHECK );
    final Button startResultProcessingCheck = m_startResultProcessingCheck;
    startResultProcessingCheck.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );
    startResultProcessingCheck.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10CalculationPage.6" ) ); //$NON-NLS-1$
    startResultProcessingCheck.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10CalculationPage.7" ) ); //$NON-NLS-1$
    startResultProcessingCheck.setSelection( m_startResultProcessing );
    startResultProcessingCheck.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        m_startResultProcessing = startResultProcessingCheck.getSelection();

        if( dialogSettings != null )
          dialogSettings.put( SETTING_START_RESULT_PROCESSING, m_startResultProcessing );
      }
    } );

    final Composite buttonComposite = new Composite( tweakGroup, SWT.NONE );
    final GridLayout layout = new GridLayout();
    layout.numColumns = 3;
    buttonComposite.setLayout( layout );
    buttonComposite.setLayoutData( new GridData( SWT.END, SWT.CENTER, true, false ) );

    final Label spinnerLabel1 = new Label( buttonComposite, SWT.NONE );
    spinnerLabel1.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10CalculationPage.8" ) ); //$NON-NLS-1$
    final GridData gridData1 = new GridData( SWT.FILL, SWT.CENTER, true, false );
    spinnerLabel1.setLayoutData( gridData1 );

    m_resultInterval = new Spinner( buttonComposite, SWT.NONE );

    final GridData gridDataSpin = new GridData( SWT.RIGHT, SWT.CENTER, true, true );

    m_resultInterval.setLayoutData( gridDataSpin );
    m_resultInterval.setDigits( 0 );
    m_resultInterval.setMinimum( 1 );
    m_resultInterval.setMaximum( 100 );
    m_resultInterval.setSelection( 1 );

    m_resultInterval.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10CalculationPage.9" ) ); //$NON-NLS-1$

    final Label spinnerLabel2 = new Label( buttonComposite, SWT.NONE );
    spinnerLabel2.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10CalculationPage.10" ) ); //$NON-NLS-1$
    final GridData gridData2 = new GridData( SWT.FILL, SWT.CENTER, true, false );
    spinnerLabel2.setLayoutData( gridData2 );

    /* Iteration viewer */
    final Group iterGroup = new Group( composite, SWT.NONE );
    iterGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    final GridLayout iterLayout = new GridLayout();
    iterGroup.setLayout( iterLayout );
    iterGroup.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10CalculationPage.11" ) ); //$NON-NLS-1$

    final Composite iterComp = new IterationComposite( m_calculation, iterGroup, SWT.NONE );
    iterComp.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    setControl( composite );
  }

  public boolean getStartResultProcessing( )
  {
    return m_startResultProcessing;
  }

  public Integer getResultInterval( )
  {
    if( m_resultInterval == null )
      return 1;
    return m_resultInterval.getSelection();
  }

  public void runCalculation( )
  {
    m_statusComp.setStatus( StatusUtilities.createStatus( IStatus.INFO, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10CalculationPage.12" ), null ) ); //$NON-NLS-1$
    setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10CalculationPage.13" ) ); //$NON-NLS-1$

    final ICoreRunnableWithProgress calculationOperation = new ICoreRunnableWithProgress()
    {
      public IStatus execute( final IProgressMonitor monitor )
      {
        return m_calculation.runCalculation( monitor );
      }
    };

    final IWizardContainer container = getContainer();
    if( container instanceof WizardDialog2 )
    {
      /* Do not block the UI while simulation is running... */
      final WizardDialog2 wd2 = (WizardDialog2) container;
      m_simulationStatus = wd2.executeUnblocked( true, true, calculationOperation );
    }
    else
      m_simulationStatus = RunnableContextHelper.execute( container, true, true, calculationOperation );

    if( m_simulationStatus.matches( IStatus.CANCEL ) ){
      /**
       * fixes the bug #242, in actual situation works only with local jobs
       * and was tested only on windows machine.
       * WPSRequest class is already signed as deprecated, so complete functionality test will not be done  
       */
      if( container instanceof WizardDialog2 )
      {
        if( !m_calculation.cancelActualJob().matches( IStatus.OK ) )
        {
          setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10CalculationPage.18" ), ERROR ); //$NON-NLS-1$
        }
        else
        {
          setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10CalculationPage.14" ), WARNING ); //$NON-NLS-1$
        }
      }
      else
      {
        setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10CalculationPage.14" ), WARNING ); //$NON-NLS-1$
      }
    }
    else if( m_simulationStatus.matches( IStatus.WARNING ) )
      setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10CalculationPage.15" ), WARNING ); //$NON-NLS-1$
    else if( m_simulationStatus.matches( IStatus.ERROR ) )
      setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10CalculationPage.16" ), ERROR ); //$NON-NLS-1$
    else
      setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10CalculationPage.17" ) ); //$NON-NLS-1$

    m_statusComp.setStatus( m_simulationStatus );

    if( !m_startResultProcessingCheck.isDisposed() )
      m_startResultProcessingCheck.setEnabled( false );
  }

  public IStatus getSimulationStatus( )
  {
    return m_simulationStatus;
  }

  public FileObject getResultDir( )
  {
    return m_calculation.getTempDir();
  }

}
