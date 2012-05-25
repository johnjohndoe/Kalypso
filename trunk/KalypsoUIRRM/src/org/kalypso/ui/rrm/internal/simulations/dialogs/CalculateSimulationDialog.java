/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.ui.rrm.internal.simulations.dialogs;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.TitleAreaDialog;
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
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;

/**
 * This dialog asks the user, if he wants to calculate the simulation and offers some options.
 * 
 * @author Holger Albert
 */
public class CalculateSimulationDialog extends TitleAreaDialog
{
  /**
   * The simulations to calculate.
   */
  private final NAControl[] m_simulations;

  /**
   * The dialog settings.
   */
  private final IDialogSettings m_settings;

  /**
   * True, if the catchment models should be calculated.
   */
  protected boolean m_calculateCatchmentModels;

  /**
   * True, if the start conditions should be calculated.
   */
  protected boolean m_calculateStartConditions;

  /**
   * The constructor.
   * 
   * @param shell
   *          The parent shell, or null to create a top-level shell.
   * @param simulations
   *          The simulations to calculate.
   */
  public CalculateSimulationDialog( final Shell shell, final NAControl[] simulations )
  {
    super( shell );

    m_simulations = simulations;
    m_settings = DialogSettingsUtils.getDialogSettings( KalypsoUIRRMPlugin.getDefault(), getClass().getName() );
    m_calculateCatchmentModels = false;
    m_calculateStartConditions = false;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createDialogArea( final Composite parent )
  {
    /* Set the title. */
    getShell().setText( "Calculate Simulations" );
    setTitle( "Calculate Simulations" );

    /* Create the main composite. */
    final Composite main = (Composite) super.createDialogArea( parent );
    main.setLayout( new GridLayout( 1, false ) );
    main.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* Create a label. */
    final Label questionLabel = new Label( main, SWT.WRAP );
    questionLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    questionLabel.setText( "Do you want to start the calculation? 'Ok' will start the calculation. 'Cancel' will abort." );

    /* Create a empty label. */
    final Label emptyLabel = new Label( main, SWT.NONE );
    emptyLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    /* Create a group. */
    final Group refreshGroup = new Group( main, SWT.NONE );
    refreshGroup.setLayout( new GridLayout( 1, false ) );
    refreshGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    refreshGroup.setText( "Refresh Simulation(s)" );

    /* Create a label. */
    final Label refreshLabel = new Label( refreshGroup, SWT.WRAP );
    refreshLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    refreshLabel.setText( "If checked, all timeseries data will be refreshed. For simulations that needs to be created, the refresh will always be done, regardless the selection here." );

    /* Create a empty label. */
    final Label emptyLabel1 = new Label( refreshGroup, SWT.NONE );
    emptyLabel1.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* Create a button. */
    final Button catchmentModelButton = new Button( refreshGroup, SWT.CHECK );
    catchmentModelButton.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    catchmentModelButton.setText( "Refresh" );
    catchmentModelButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final Button source = (Button) e.getSource();
        m_calculateCatchmentModels = source.getSelection();
      }
    } );

    /* Show the group, only if there is a longterm simulation. */
    if( !containsLongtermSimulation( m_simulations ) )
      return main;

    /* Create a group. */
    final Group startConditionsGroup = new Group( main, SWT.NONE );
    startConditionsGroup.setLayout( new GridLayout( 1, false ) );
    startConditionsGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    startConditionsGroup.setText( "Calculate Start Conditions" );

    /* Create a label. */
    final Label startConditionsLabel = new Label( startConditionsGroup, SWT.WRAP );
    startConditionsLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    startConditionsLabel.setText( "If checked, the start conditions will be calculated. This affects only longterm simulations." );

    /* Create a empty label. */
    final Label emptyLabel2 = new Label( startConditionsGroup, SWT.NONE );
    emptyLabel2.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* Create a button. */
    final Button startConditionsButton = new Button( startConditionsGroup, SWT.CHECK );
    startConditionsButton.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    startConditionsButton.setText( "Calculate" );
    startConditionsButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final Button source = (Button) e.getSource();
        m_calculateStartConditions = source.getSelection();
      }
    } );

    // TODO NTH Erst mal nur als INFO in Tabelle oder FV
    // TODO Simulation wurde bereits gerechnet -> Gebietsmodelle sind nicht aktuell und m�ssen neu gerechnet werden...
    // TODO Simulation wurde bereits gerechnet -> Gebietsmodelle sind aktuell und m�ssen nicht neu gerechnet werden...
    // TODO Vergleiche lastModified von den GNMs und den Simulationsergebnis...

    return main;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#getDialogBoundsSettings()
   */
  @Override
  protected IDialogSettings getDialogBoundsSettings( )
  {
    return DialogSettingsUtils.getSection( m_settings, "bounds" ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#isResizable()
   */
  @Override
  protected boolean isResizable( )
  {
    return true;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#okPressed()
   */
  @Override
  protected void okPressed( )
  {
    /* Nothing to do. */

    super.okPressed();
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#cancelPressed()
   */
  @Override
  protected void cancelPressed( )
  {
    m_calculateCatchmentModels = false;
    m_calculateStartConditions = false;

    super.cancelPressed();
  }

  /**
   * This function returns true, if one longterm simulation is contained.
   * 
   * @param simulations
   *          The simulations to calculate.
   */
  private boolean containsLongtermSimulation( final NAControl[] simulations )
  {
    for( final NAControl simulation : simulations )
    {
      final Integer timestep = simulation.getMinutesOfTimestep();
      if( timestep == null )
        continue;

      if( timestep.intValue() == 1440 )
        return true;
    }

    return false;
  }

  /**
   * This function returns true, if the catchment models should be calculated.
   * 
   * @return True, if the catchment models should be calculated.
   */
  public boolean isCalculateCatchmentModels( )
  {
    return m_calculateCatchmentModels;
  }

  /**
   * This function returns true, if the start conditions should be calculated.
   * 
   * @return True, if the start conditions should be calculated.
   */
  public boolean isCalculateStartConditions( )
  {
    return m_calculateStartConditions;
  }
}