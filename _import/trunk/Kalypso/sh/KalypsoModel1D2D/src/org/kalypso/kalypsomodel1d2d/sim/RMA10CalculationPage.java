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
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;

/**
 * @author Gernot Belger
 */
public class RMA10CalculationPage extends WizardPage implements IWizardPage
{
  private final RMA10Calculation m_calculation;

  private IStatus m_simulationStatus;

  private StatusComposite m_statusComp;

  protected boolean m_startResultProcessing = false;

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
    final Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout() );

    /* Status composite */
    m_statusComp = new StatusComposite( composite, SWT.NONE );
    m_statusComp.setLayoutData( new GridData( SWT.FILL, SWT.LEFT, true, false ) );

    /* Control flags */
    final Button startResultProcessingCheck = new Button( composite, SWT.CHECK );
    startResultProcessingCheck.setText( "Sofort mit Ergebnisauswertung starten" );
    startResultProcessingCheck.setToolTipText( "Falls gesetzt, wird nach der Simulation sofort mit der Ergebnisauswertug aller Zeitschritte gestartet." );
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
      }
    } );

    /* Iteration viewer */
    final Composite iterComp = new Composite( composite, SWT.NONE );
    iterComp.setLayout( new GridLayout( 2, false ) );
    iterComp.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    setControl( composite );
  }

  public boolean getStartResultProcessing( )
  {
    return m_startResultProcessing;
  }

  public void runCalculation( )
  {
    setMessage( "Simulation in Arbeit..." );

    final RMA10Calculation calculation = m_calculation;
    final ICoreRunnableWithProgress calculationOperation = new ICoreRunnableWithProgress()
    {
      public IStatus execute( final IProgressMonitor monitor )
      {
        return calculation.runCalculation( monitor );
      }
    };

    m_simulationStatus = RunnableContextHelper.execute( getContainer(), true, true, calculationOperation );

    setMessage( "Drücken Sie auf 'Weiter', um die Simulationsergebnisse auszuwerten." );

    m_statusComp.setStatus( m_simulationStatus );
  }

  // private static int convertSeverity( final int severity )
  // {
  // switch( severity )
  // {
  // case IStatus.CANCEL:
  // return IMessageProvider.NONE;
  //
  // case IStatus.ERROR:
  // return IMessageProvider.ERROR;
  //
  // case IStatus.INFO:
  // return IMessageProvider.INFORMATION;
  //
  // case IStatus.OK:
  // return IMessageProvider.INFORMATION;
  //
  // case IStatus.WARNING:
  // return IMessageProvider.WARNING;
  //
  // default:
  // return IMessageProvider.NONE;
  // }
  // }

  public IStatus getSimulationStatus( )
  {
    return m_simulationStatus;
  }

}
