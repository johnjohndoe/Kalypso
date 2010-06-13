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

import org.apache.commons.vfs.FileObject;
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
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit.TYPE;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.geolog.IGeoLog;
import org.kalypso.service.wps.client.WPSRequest;
import org.kalypso.util.swt.StatusComposite;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

/**
 * @author Gernot Belger
 */
public class RMA10CalculationPage extends WizardPage implements IWizardPage
{
  private static final String SETTING_START_RESULT_PROCESSING = "startResultProcessing"; //$NON-NLS-1$

  // protected final RMAKalypsoSimulationRunner m_calculation;
  private final IControlModel1D2D m_controlModel;

  private StatusComposite m_statusComp;

  protected boolean m_startResultProcessing = false;

  protected boolean m_isCoupledSimulation = false;

  private Button m_startResultProcessingCheck;

  private Spinner m_resultInterval;

  protected Button m_coupledSimulationCheck;

  private final IGeoLog m_geoLog;

  private Group m_iterGroup;

  protected RMAKalypsoSimulationRunner m_calculation;

  protected RMA10CalculationPage( final String pageName, final IGeoLog geoLog, final IControlModel1D2D controlModel )
  {
    super( pageName );
    m_geoLog = geoLog;
    m_controlModel = controlModel;

    final ICalculationUnit calculationUnit = controlModel.getCalculationUnit();
    setTitle( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10CalculationPage.0", calculationUnit.getName() ) ); //$NON-NLS-1$
    setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10CalculationPage.2" ) ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
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
    final GridLayout tweakLayout = new GridLayout( 3, false );
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
    buttonComposite.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, true, false ) );

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
    final ICalculationUnit calculationUnit = m_controlModel.getCalculationUnit();

    m_iterGroup = new Group( composite, SWT.NONE );
    m_iterGroup.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10CalculationPage.11" ) ); //$NON-NLS-1$

    m_coupledSimulationCheck = new Button( tweakGroup, SWT.CHECK );
    m_coupledSimulationCheck.setLayoutData( new GridData( SWT.END, SWT.CENTER, false, false ) );
    m_coupledSimulationCheck.setText( "Coupled simulation" );
    m_coupledSimulationCheck.setEnabled( false );
    m_coupledSimulationCheck.setToolTipText( "Run a coupled simulation with separate processes for each subunit." );
    if( calculationUnit.getType() != TYPE.TYPE1D2D )
    {
      m_isCoupledSimulation = false;
      m_coupledSimulationCheck.setEnabled( false );
    }
    else
    {
      m_isCoupledSimulation = false;
      m_coupledSimulationCheck.setEnabled( false );
      // m_isCoupledSimulation = ((ICoupledCalculationUnit) calculationUnit).isCoupledSimulation();
      // m_coupledSimulationCheck.setSelection( m_isCoupledSimulation );
      // m_coupledSimulationCheck.addSelectionListener( new SelectionAdapter()
      // {
      // /**
      // * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
      // */
      // @Override
      // public void widgetSelected( final SelectionEvent e )
      // {
      // m_isCoupledSimulation = m_coupledSimulationCheck.getSelection();
      // ((ICoupledCalculationUnit) calculationUnit).setCoupledSimulation( m_isCoupledSimulation );
      // }
      // } );
    }
    updateIterationViewerGroup( null );

    setControl( composite );
  }

  protected void updateIterationViewerGroup( final RMAKalypsoSimulationRunner calculation )
  {
    // remove old controls
    final Control[] children = m_iterGroup.getChildren();
    if( children.length > 0 )
    {
      for( final Control iterComp : children )
      {
        iterComp.dispose();
      }
    }

    if( calculation == null )
    {
      m_iterGroup.setLayout( new GridLayout() );
      m_iterGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    }
    else
    {
      final ICalculationUnit calcUnit = calculation.getControlModel().getCalculationUnit();

      if( isCoupledSimulation() )
      {
        // if coupled simulation is selected, display one iteration composite per subunit
        final ICalculationUnit1D2D calculationUnit1D2D = (ICalculationUnit1D2D) calcUnit;
        final IFeatureWrapperCollection<ICalculationUnit> subUnits = calculationUnit1D2D.getChangedSubUnits();

        final int numSubunits = subUnits.size();
        m_iterGroup.setLayout( new GridLayout( (int) Math.ceil( numSubunits / 2.0 ), true ) );
        m_iterGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

        final Control[] newIterComps = new Control[numSubunits];
        for( int i = 0; i < numSubunits; i++ )
        {
          final ICalculationUnit subUnit = subUnits.get( i );
          newIterComps[i] = new IterationComposite( m_iterGroup, calculation, subUnit, SWT.NONE );
          newIterComps[i].setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
        }
      }
      else
      {
        // if no coupled simulation is selected, display only one iteration composite
        m_iterGroup.setLayout( new GridLayout() );
        m_iterGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

        final Composite iterComp = new IterationComposite( m_iterGroup, calculation, calcUnit, SWT.NONE );
        iterComp.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
      }
    }
    // refresh
    m_iterGroup.layout();
  }

  public boolean getStartResultProcessing( )
  {
    return m_startResultProcessing;
  }

  public boolean isCoupledSimulation( )
  {
    return m_isCoupledSimulation;
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

    // default to local simulation
    String serviceEndpoint = System.getProperty( "org.kalypso.service.wps.service" ); //$NON-NLS-1$
    if( serviceEndpoint == null || serviceEndpoint.equals( "" ) ) //$NON-NLS-1$
    {
      serviceEndpoint = WPSRequest.SERVICE_LOCAL;
    }

    if( m_isCoupledSimulation )
    {
      // final ICalculationUnit1D2D calculationUnit1d2d = (ICalculationUnit1D2D) m_controlModel.getCalculationUnit();
      // final ICalculationUnit calculationUnit = calculationUnit1d2d.getChangedSubUnits().get( 0 );
      m_calculation = new RMAKalypsoSimulationRunner( m_geoLog, m_controlModel, serviceEndpoint );
    }
    else
    {
      m_calculation = new RMAKalypsoSimulationRunner( m_geoLog, m_controlModel, serviceEndpoint );
    }
    updateIterationViewerGroup( m_calculation );

    final ICoreRunnableWithProgress calculationOperation = new ICoreRunnableWithProgress()
    {
      @Override
      public IStatus execute( final IProgressMonitor monitor )
      {
        monitor.beginTask( "", 1 );
        try
        {
          return m_calculation.runCalculation( monitor );
        }
        finally
        {
          monitor.done();
        }
      }
    };

    final IWizardContainer container = getContainer();
    if( container instanceof WizardDialog2 )
    {
      /* Do not block the UI while simulation is running... */
      final WizardDialog2 wd2 = (WizardDialog2) container;
      wd2.executeUnblocked( true, true, calculationOperation );
    }
    else
      RunnableContextHelper.execute( container, true, true, calculationOperation );

    final IStatus simulationStatus = getSimulationStatus();
    if( simulationStatus.matches( IStatus.CANCEL ) )
      setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10CalculationPage.14" ), WARNING ); //$NON-NLS-1$
    else if( simulationStatus.matches( IStatus.WARNING ) )
      setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10CalculationPage.15" ), WARNING ); //$NON-NLS-1$
    else if( simulationStatus.matches( IStatus.ERROR ) )
      setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10CalculationPage.16" ), ERROR ); //$NON-NLS-1$
    else
      setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.RMA10CalculationPage.17" ) ); //$NON-NLS-1$

    m_statusComp.setStatus( simulationStatus );

    if( !m_startResultProcessingCheck.isDisposed() )
      m_startResultProcessingCheck.setEnabled( false );

    if( !m_coupledSimulationCheck.isDisposed() )
      m_coupledSimulationCheck.setEnabled( false );
  }

  public IStatus getSimulationStatus( )
  {
    return m_calculation != null ? m_calculation.getSimulationStatus() : null;
  }

  public FileObject getResultDir( )
  {
    return m_calculation != null ? m_calculation.getTempDir() : null;
  }
}
