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
package org.kalypso.kalypsomodel1d2d.sim;

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
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.util.swt.StatusComposite;

/**
 * @author Gernot Belger
 */
public class RMA10CalculationPage extends WizardPage implements IWizardPage
{
  private static final String SETTING_START_RESULT_PROCESSING = "startResultProcessing";

  private final RMA10Calculation m_calculation;

  private IStatus m_simulationStatus;

  private StatusComposite m_statusComp;

  protected boolean m_startResultProcessing = false;

  private Button m_startResultProcessingCheck;

  protected RMA10CalculationPage( final String pageName, final RMA10Calculation calculation )
  {
    super( pageName );

    m_calculation = calculation;

    setTitle( "Simulation - " + m_calculation.getControlModel().getCalculationUnit().getName() );

    setMessage( "Drücken Sie 'Start', um die Simulation zu starten." );
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
    statusGroup.setText( "Simulationsergebnis" );
    m_statusComp = new StatusComposite( statusGroup, StatusComposite.DETAILS );
    m_statusComp.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    m_statusComp.setStatus( StatusUtilities.createStatus( IStatus.INFO, "Simulation wurde noch nicht durchgeführt", null ) );

    final Group tweakGroup = new Group( composite, SWT.NONE );
    tweakGroup.setLayout( new GridLayout() );
    tweakGroup.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    tweakGroup.setText( "Einstellungen" );

    m_startResultProcessingCheck = new Button( tweakGroup, SWT.CHECK );
    final Button startResultProcessingCheck = m_startResultProcessingCheck;
    startResultProcessingCheck.setLayoutData( new GridData( SWT.BEGINNING, SWT.BEGINNING, false, false ) );
    startResultProcessingCheck.setText( "Sofort mit der Ergebnisauswertung starten" );
    startResultProcessingCheck.setToolTipText( "Falls gesetzt, wird nach der Simulation sofort mit der Ergebnisauswertung aller Zeitschritte gestartet." );
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

    /* Iteration viewer */
    final Group iterGroup = new Group( composite, SWT.NONE );
    iterGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    final GridLayout iterLayout = new GridLayout();
    iterGroup.setLayout( iterLayout );
    iterGroup.setText( "Iterationsverlauf" );

    final Composite iterComp = new IterationComposite( m_calculation, iterGroup, SWT.NONE );
    iterComp.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    setControl( composite );
  }

  public boolean getStartResultProcessing( )
  {
    return m_startResultProcessing;
  }

  public void runCalculation( )
  {
    m_statusComp.setStatus( StatusUtilities.createStatus( IStatus.INFO, "Simulation läuft...", null ) );
    setMessage( "Simulation läuft..." );

    final RMA10Calculation calculation = m_calculation;
    final ICoreRunnableWithProgress calculationOperation = new ICoreRunnableWithProgress()
    {
      public IStatus execute( final IProgressMonitor monitor )
      {
        return calculation.runCalculation( monitor );
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

    if( m_simulationStatus.matches( IStatus.CANCEL ) )
      setMessage( "Simulation abgebrochen.", WARNING );
    else if( m_simulationStatus.matches( IStatus.WARNING ) )
      setMessage( "Simulation mit Warnung beendet. Drücken Sie auf 'Weiter', um die Simulationsergebnisse auszuwerten.", WARNING );
    else if( m_simulationStatus.matches( IStatus.ERROR ) )
      setMessage( "Simulation mit Fehler beendet. Ergebnisauswertung nicht möglich.", ERROR );
    else
      setMessage( "Drücken Sie auf 'Weiter', um die Simulationsergebnisse auszuwerten." );

    m_statusComp.setStatus( m_simulationStatus );

    if( !m_startResultProcessingCheck.isDisposed() )
      m_startResultProcessingCheck.setEnabled( false );
  }

  public IStatus getSimulationStatus( )
  {
    return m_simulationStatus;
  }

}
